<?php

class ClusterMaps 
{
    public function loadData($map_files)
    {
        try {
            $sessionid = session_id();
            //store filesname in a text file
            $filenames_file = __DIR__ . "/../public/Uploads/" . $sessionid . "FileNames.csv";
            $file = fopen($filenames_file,"w");
            //$file = fopen($sessionid."FileNames.csv","w");
            fputcsv($file,$map_files);
            fclose($file);

            $sel = $_SESSION["filesWereUploaded_maps"];
            exec("Rscript loadData_maps.R $sel $sessionid");

            //wait for file with name mi[sessionid].txt and can[sessionid].txt to appear then 1) copy-paste-rename and delete it. 2) Read contents of file to populate two dropdowns
            $resultsfile_mi = __DIR__ . "/../public/Results/mi" . $sessionid . ".txt";
            $resultsfile_can = __DIR__ . "/../public/Results/can" . $sessionid . ".txt";

            $renamedfile_mi = __DIR__ . "/../public/Results/datami" . $sessionid . ".txt";
            $renamedfile_can = __DIR__ . "/../public/Results/datacan" . $sessionid . ".txt";

            while(! (file_exists($resultsfile_mi) && file_exists($resultsfile_can)) ) sleep(1);

            copy($resultsfile_mi, $renamedfile_mi);
            copy($resultsfile_can, $renamedfile_can);

            unlink($resultsfile_mi);
            unlink($resultsfile_can);

            $mi_data = file_get_contents($renamedfile_mi);
            $mi_lines = explode("\n", $mi_data);
            $_SESSION["mi_lines"] = $mi_lines;

            $can_data = file_get_contents($renamedfile_can);
            $can_lines = explode("\n", $can_data);
            $_SESSION["can_lines"] = $can_lines;
        } catch(Exception $e) {
            $logger->addInfo("ERROR- " . $e);
        }        
    }

    private function plotsExist($filesToExpect)
    {
        $dir = "D:/PHP/ElectionForensics/public/Results/";

        $sessionid = session_id();

        $files = array();
        foreach (glob($dir.$sessionid."*.png") as $file) {
	        $files[] = $file;
        }

        if (count($files) == $filesToExpect) {
            return true;
        } else {
            return false;
        }
    }


    public function doAnalysis($analysis_data_maps, $logger)
    {
	    try {
            $logger->addInfo("clustermas do analysis method invoked");
            // get unique session id for creating unique file names
            $sessionid = session_id();
	    
            // execute R script from shell
	        // this will save results to the filesystem

            //save data to csv file
            //$fdata = $_SESSION["csv_data"];

            //$jsonfile = "data" . $sessionid . ".csv";
            
            //$fp = fopen($jsonfile, 'w');
            //fwrite($fp, $fdata);
            //fclose($fp);

            //$fname = $_SESSION["userSelectedFile"];

            $mi = $analysis_data_maps->getMergeIndex();
            $can = $analysis_data_maps->getCandidates();
	        $met = $analysis_data_maps->getMethods();
            
//            $logger->addInfo("fname- " . $fname);
//            $logger->addInfo("jsonfile- " . $jsonfile);
            $logger->addInfo("mi- " . $mi);
            $logger->addInfo("can- " . $can);
            $logger->addInfo("met- " . $met);

            $sel = $_SESSION["filesWereUploaded_maps"];

            try {
                exec("Rscript invokeMaps.R $sel $sessionid $mi $can $met");
            } catch(Exception $e) {
                $logger->addInfo("ERROR- " . $e);
            }

            //wait for a list of files with names [sessionid][i].png and [sessionid][i]_2.png to appear, for each candidate selected. So if one candidate was selected, there will be two files. 
            //then 
            //1) copy-paste-rename and delete it. 
            //2) show button for download 
            //3) allow user to download results[sessionid].csv
            //$resultsfile = $sessionid . ".csv";

            $no_files = count($can) * 2;

            while(!$this->plotsExist($no_files)) sleep(2);

            //$dir = "";
            $dir = "D:/PHP/ElectionForensics/public/Results/";
            $files = array();
            foreach (glob($dir.$sessionid."*.png") as $file) {
	            $files[] = $file;
            }

            foreach($files as $file) {
	            copy($file, str_replace($sessionid, "plot" . $sessionid, $file));
            }

            foreach($files as $file) {
	             unlink($file);
            }
            $images = glob($dir."plot".$sessionid."*.png");
            return $images;
            /*$images = glob($dir."plot".$sessionid."*.png");
            foreach($images as $image) {
	            echo $image;
	            echo '<img src="'.$image.'" /><br />';
            }*/

	    }
	    catch (Exception $e) {
		    echo 'caught exception: ', $e->getMessage(), "\n";
	    }        
    }

}

?>