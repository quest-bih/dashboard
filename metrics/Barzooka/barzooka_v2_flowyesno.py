import os
import glob
import requests
from fastai.vision.all import *
import urllib3
urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

os.environ['NO_PROXY'] = '127.0.0.1'


class Barzooka(object):
    def __init__(self, model_file='barzooka.pkl'):

        super(Barzooka, self).__init__()
        self.learner = load_learner(model_file)
        self.iiif_url = "http://127.0.0.1:8182/iiif/2/" + \
                        "{}:{}.pdf/full/560,560/0/default.png?page={}"
        self.re_pg = re.compile(r'Index: \d+, Size: (\d+)')

    def predict_from_folder(self, pdf_folder, save_filename,
                            iiif_folder='', iiif_mode=False,
                            tmp_folder='./tmp/'):
        """Barzooka prediction for folder of publication pdf files"""
        if(iiif_folder == '' and iiif_mode):
            raise ValueError("iiif folder argument missing")
        if(tmp_folder == '' and not iiif_mode):
            raise ValueError("tmp folder argument missing")
        if not os.path.exists(tmp_folder) and not iiif_mode:
            os.mkdir(tmp_folder)

        pdf_table = self.__get_pdf_list(pdf_folder, iiif_mode)
        with open(save_filename, "w") as f:
            f.write("approp,bar,pie,hist,bardot,box,dot,violin,flowyes,flowno,paper_id\n")
        for index, row in pdf_table.iterrows():
            paper_id = row['paper_id']
            print(paper_id)
            try:
                if(iiif_mode):
                    barzooka_result = self.__detection_iiif(paper_id, iiif_folder)
                else:
                    barzooka_result = self.predict_from_file(paper_id, tmp_folder)
            except:
                print("Could not screen pdf " + paper_id)
                # remove potentially remaining images
                images = get_image_files(tmp_folder)
                for j in range(0, len(images)):
                    os.remove(images[j])
                print("Removed remaining images " + paper_id)
                

            result_row = pd.DataFrame([barzooka_result])
            result_row.to_csv(save_filename, mode='a', header=False, index=False)

    def predict_from_file(self, pdf_file, tmp_folder, pagewise=False):
        """Barzooka prediction for publication pdf files"""
        # convert pdf to images
        self.__convert_pdf(pdf_file, tmp_folder)
     	
     	# load images
        images = get_image_files(tmp_folder)

        # predict on images
        classes_detected = self.__predict_img_list(images, pagewise)
        doi = pdf_file.split('/')[-1].replace("+", "/").replace(".pdf", "")
        if pagewise == False:
            classes_detected['paper_id'] = doi

        # remove images again
        for j in range(0, len(images)):
            os.remove(images[j])

        return classes_detected

    def predict_from_img(self, img_files):
        """Barzooka prediction for list of image files"""
        images = [open_image(img) for img in img_files]

        # predict on images
        classes_detected = self.__predict_img_list(images, pagewise = True)
        return classes_detected
        
    def predict_from_img_folder(self, img_folder):
        """Barzooka prediction for folder of image files"""
        images = get_image_files(img_folder)

        # predict on images
        classes_detected = self.__predict_img_list(images, pagewise = True)
        return [images, classes_detected]

    def __get_pdf_list(self, pdf_folder, iiif_mode=True):
        """Searches PDF folder for all PDF filenames and returns them
           as dataframe"""
        pdf_list = []
        for root, dirs, files in os.walk(pdf_folder):
            for filename in files:
                if(iiif_mode):
                    paper_dict = {"paper_id": filename[:-4].replace("+", "%2b")}
                else:
                    paper_dict = {"paper_id": root + filename}
                pdf_list.append(paper_dict)

        pdf_table = pd.DataFrame(pdf_list)
        return pdf_table

    def __convert_pdf(self, pdf_file, tmp_folder):
        image_filename = pdf_file.split('/')[-1][:-4]
        os.system('pdftocairo -jpeg -scale-to-x 560 -scale-to-y 560 "'
                  + pdf_file + '" "' + tmp_folder + image_filename + '"')

    def __detection_iiif(self, paper_id, folder, debug=False, pagewise=False):
        """Pull images from iiif server"""
        pages = self.__count_pages(paper_id, folder)
        if pages == 0:
            return self.__empty_result(paper_id, folder)
        images = [open_image(io.BytesIO(requests.get(self.iiif_url.format(folder, paper_id, pg)).content))
                  for pg in range(1, pages + 1)]
        classes_detected = self.__predict_img_list(images, pagewise)
        classes_detected['paper_id'] = paper_id.replace("%2b", "/")
        return classes_detected

    def __empty_result(self, paper_id, folder):
        """If pdf with paper_id not available"""
        classes_detected = {
            'approp': 0,
            'bar': 0,
            'pie': 0,
            'hist': 0,
            'bardot': 0,
            'box': 0,
            'dot': 0,
            'violin': 0,
            'flowyes': 0,
            'flowno': 0,
            'paper_id': paper_id.replace("%2b", "/")
        }
        return classes_detected

    def __predict_img_list(self, images, pagewise):
        """Predicts graph types for each image & returns pages with bar graphs
        """
        page_predictions = [self.__predict_graph_type(images[idx])
                                     for idx in range(0, len(images))]
        class_names = ['approp', 'bar', 'pie', 'hist', 'bardot', 'box', 'dot', 'violin', 'flowyes','flowno']
        class_counts = [self.__count_class(class_name, page_predictions) for class_name in class_names]
        classes_detected = dict(zip(class_names, class_counts))
        if(pagewise):
            return page_predictions
        return classes_detected

    def __count_class(self, class_name, predictions):
        return [class_name in page for page in predictions].count(True) 

    def __predict_graph_type(self, img):
        """Use fastai model on each image to predict types of pages
        """
        class_names = {
            "0": ["approp"],
            "1": ["bar"],
            "2": ["bardot"],
            "3": ["box"],
            "4": ["dot"],
            "5": ["flowno"],
            "6": ["flowyes"],
            "7": ["hist"],
            "8": ["other"],
            "9": ["pie"],
            "10": ["text"],
            "11": ["violin"]
        }
        pred_class, pred_idx, outputs = self.learner.predict(img)
        if pred_idx.sum().tolist() == 0:
            # if there is no predicted class (=no class over threshold)
            # give out class with highest prediction probability
            highest_pred = str(np.argmax(outputs).tolist())
            pred_class = class_names[highest_pred]
        else:
            pred_class = pred_class.items  # extract class name as text
        return(pred_class)

    def __count_pages(self, paper_id, folder):
        """cantaloupe iiif server returns the highest page index with an error
        if out of range is requested"""
        url = self.iiif_url.format(folder, paper_id, "1000")
        page = self.__req_internal(url)
        try:
            count = self.re_pg.findall(page)[0]
        except:
            count = 0
        return int(count)

    def __req_internal(self, url):
        http = urllib3.PoolManager(cert_reqs='CERT_NONE')
        page = http.request('get', url, timeout=120)
        return page.data.decode('utf-8')
