<?php 
class BasicForensicsAnalysis 
{
    public function doAnalysis($analysis_data, $logger)
    {
	    try {
            $logger->addInfo("do analysis method invoked");
            // get unique session id for creating unique file names
            $sessionid = session_id();
	    
            // execute R script from shell
	        // this will save results to the filesystem

          //  //save data to csv file moved to after file upload, only need the file name
          //   $fdata = $_SESSION["csv_data"];

            $jsonfile = "data" . $sessionid . ".csv";
          //  //$jsonfile = __DIR__ . "/../public/Results/data" . $sessionid . ".csv";
            
         //   $fp = fopen($jsonfile, 'w');
         //   fwrite($fp, $fdata);
         //   fclose($fp);

            $fname = $_SESSION["userSelectedFile"];

            $can = $analysis_data->getCandidates();
            $lev = $analysis_data->getLevel();
            $reg = $analysis_data->getRegistered();
            $vot = $analysis_data->getVotes();
	        $met = $analysis_data->getMethods();
            
            $logger->addInfo("fname- " . $fname);
            $logger->addInfo("jsonfile- " . $jsonfile);
            $logger->addInfo("can- " . $can);
            $logger->addInfo("lev- " . $lev);
            $logger->addInfo("reg- " . $reg);
            $logger->addInfo("vot- " . $vot);
            $logger->addInfo("met- " . $met);

            try {
                exec("Rscript invokeEF.R $fname $jsonfile $can $lev $reg $vot $met");
            } catch(Exception $e) {
                $logger->addInfo("ERROR- " . $e);
            }

            //wait for file with name [sessionid].csv to appear then 1) copy-paste-rename and delete it. 2) show button for download 3) allow user to download results[sessionid].csv
            $resultsfile = $sessionid . ".csv";
            //$resultsfile = __DIR__ . "/../public/Results/" . $sessionid . ".csv";
            while(!file_exists($resultsfile)) sleep(1);

            $renamedfile = "results" . $resultsfile;                 
            //$resultsfile = __DIR__ . "/../public/Results/results" . $sessionid . ".csv";

            copy($resultsfile, $renamedfile);

            //do same for html file
            $resultsfile_html = $sessionid . ".html";
            //$resultsfile_html = __DIR__ . "/../public/Results/" . $sessionid . ".html";
            $renamedfile_html = "results" . $resultsfile_html;          
            //$renamedfile_html = __DIR__ . "/../public/Results/results" . $sessionid . ".html";
            copy($resultsfile_html, $renamedfile_html);

            unlink($resultsfile);
            unlink($resultsfile_html);
	    }
	    catch (Exception $e) {
		    echo 'caught exception: ', $e->getMessage(), "\n";
	    }        
    }

}

?>