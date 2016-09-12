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

            // name of file where data resides 
            $csvfilename = "data" . $sessionid . ".csv";

            $fname = $_SESSION["userSelectedFile"];

            $can = $analysis_data->getCandidates();
            $lev = $analysis_data->getLevel();
            $reg = $analysis_data->getRegistered();
            $vot = $analysis_data->getVotes();
	        $met = $analysis_data->getMethods();
            
            $logger->addInfo("fname- " . $fname);
            $logger->addInfo("csvfilename- " . $csvfilename);
            $logger->addInfo("can- " . $can);
            $logger->addInfo("lev- " . $lev);
            $logger->addInfo("reg- " . $reg);
            $logger->addInfo("vot- " . $vot);
            $logger->addInfo("met- " . $met);

            try {
                exec("Rscript invokeEF.R $fname $csvfilename $can $lev $reg $vot $met");
            } catch(Exception $e) {
                $logger->addInfo("ERROR- " . $e);
            }

            //wait for file with name [sessionid].csv to appear then 1) copy-paste-rename and delete it. 2) show button for download 3) allow user to download results[sessionid].csv
            //$resultsfile = $sessionid . ".csv";
            $resultsfile = __DIR__ . "/../public/Results/" . $sessionid . ".csv";
            $renamedfile = __DIR__ . "/../public/Results/results" . $sessionid . ".csv";

            $resultsfile_html = __DIR__ . "/../public/Results/" . $sessionid . ".html";
            $renamedfile_html = __DIR__ . "/../public/Results/results" . $sessionid . ".html";

            while(!file_exists($resultsfile)) sleep(1);

            copy($resultsfile, $renamedfile);
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