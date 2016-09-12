<?php 

class FileManager 
{
    public function determineFilename($cntry) 
    {
	    if ($cntry != "") 
        {
             $base_dir = "D:/PHP/ElectionForensics/public/Data/";

		    switch ($cntry) 
            {
		        case "Afghanistan 2014 initial":
		            $fn = $base_dir . "Afghanistan2014/votes/2014_afghanistan_election_results.csv";
		            break;
		        case "Afghanistan 2014 runoff":
		            $fn = $base_dir . "Afghanistan2014/votes/2014_afghanistan_preliminary_runoff_election_results.csv";
		            break;
		        case "Albania 2013":
		            $fn = $base_dir . "Albania2013/votes/Albania2013.csv";
		            break;
		        case "Bangladesh 2001":
		            $fn = $base_dir . "Bangladesh2001/votes/Election_Data_2001.csv";
		            break;
		        case "Cambodia 2013":
		            $fn = $base_dir . "Cambodia2013/votes/Cambodia2013.csv";
		            break;
		        case "Kenya 2013":
		            $fn = $base_dir . "Kenya2013/votes/Kenya2013.csv";
		            break;
		        case "Libya 2013":
		            $fn = $base_dir . "Libya2014/votes/Libya2014CC.csv";
		            break;
		        case "Libya 2013 Fem":
		            $fn = $base_dir . "Libya2014/votes/Libya2014CC_F.csv";
		            break;
		        case "South Africa 2014":
		            $fn = $base_dir . "SouthAfrica2014/votes/SouthAfrica2014.csv";
		            break;
		        case "Uganda 2006":
		            $fn = $base_dir . "Uganda2006/votes/2006_pres_pollingDC.csv";
		            break;
		    }
	    } 
        else 
        {
		    $fn = $_FILES['userfile']['tmp_name'];
	    }
	    return $fn;
    }
    
    public function getFileContents($cntry, $logger) 
    {
        $filename = $this->determineFilename($cntry);
        $logger->addInfo("getFileContents filename: " . $filename);                    
        $csv_data = file_get_contents($filename);

        $lines = explode("\n", $csv_data);
        $head = str_getcsv(array_shift($lines));

        $array = array();
        foreach ($lines as $line) 
        {
            $array[] = array_combine($head, str_getcsv($line));
        }

        $_SESSION["array"] = $array;  // save data in session so it can be passed to ElectionForensics when user clicks Start Analysis
        $_SESSION["head"] = $head;
        $_SESSION["csv_data"] = $csv_data;

        $sessionid = session_id();
        // call R file to create summary
        try {
              //$fname = $_SESSION["userSelectedFile"];
              //$logger->addInfo("userSelectedFile filename: " . $fname);                    

                //save data to csv file
                $fdata = $_SESSION["csv_data"];

                $jsonfile = "data" . $sessionid . ".csv";
                //$jsonfile = __DIR__ . "/../public/Results/data" . $sessionid . ".csv";
            
                $fp = fopen($jsonfile, 'w');
                fwrite($fp, $fdata);
                fclose($fp);


              exec("Rscript getSummary.R $jsonfile $sessionid");
        } catch(Exception $e) {
            $logger->addInfo("ERROR- " . $e);
        }
        
        //wait for file with name [sessionid].txt to appear then 1) copy-paste-rename and delete it. 
        $summary_file = $sessionid . ".txt";
        //$resultsfile = __DIR__ . "/../public/Results/" . $sessionid . ".csv";
        while(!file_exists($summary_file)) sleep(1);

        $summary_renamedfile = "summary_" . $summary_file;                 
        //$resultsfile = __DIR__ . "/../public/Results/results" . $sessionid . ".csv";

        copy($summary_file, $summary_renamedfile);

        unlink($summary_file);
        
    }
}

?>