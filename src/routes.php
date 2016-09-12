<?php

$app->get('/logout', function ($request, $response, $args) {
    $this->logger->addInfo("logout user");
    //unset($_SESSION['user']);
    session_unset();
    $this->logger->addInfo("USER LOGGED OUT");
    return $this->renderer->render($response, 'index.phtml', $args);
});
    
$app->post('/loginuser', function ($request, $response, $args) {
    $this->logger->addInfo("login user");

    $data = $request->getParsedBody();
    $email = filter_var($data['email'], FILTER_SANITIZE_STRING);
    $pass = filter_var($data['pass'], FILTER_SANITIZE_STRING);
    
    if(empty($email)){
        $error = true;
        $emailError = "Please enter your email address.";
    } else if ( !filter_var($email,FILTER_VALIDATE_EMAIL) ) {
        $error = true;
        $emailError = "Please enter valid email address.";
    }
  
    if(empty($pass)){
        $error = true;
        $passError = "Please enter your password.";
    }
  
    // if there's no error, continue to login
    if (!$error) {
   
        $pass = hash('sha256', $pass); // password hashing using SHA256
  
        // Create connection
        $conn = new mysqli('isr-sqlpika.isr.umich.edu', 'eftuser', 'eftuser', 'eft');

        // Check connection
        if ($conn->connect_error) {
            die("Connection failed: " . $conn->connect_error);
        } 
        //if (!mysqli_select_db($conn, $database)) {
          //  die("Uh oh, couldn't select database $database");
        //}
        
        $sql = "SELECT * FROM users WHERE userEmail='$email'";
        if(!$result = $conn->query($sql)){
            die('There was an error running the query [' . $conn->error . ']');
        }
        // there should be only 1
        $row = $result->fetch_assoc();
        $this->logger->addInfo("user info-" . $row['userEmail'] . ' : ' . $row['userPass']);
        if ($row['userPass']==$pass) {
            $_SESSION['user'] = $row['userEmail'];     
            $errMSG = "You are successfully logged in.";
        } else {
            $errMSG = "Incorrect Credentials, Try again...";
        }
        $result->close();
    }
    
    return $this->renderer->render($response, 'index.phtml', array('passError2' => $passError, 'emailError2' => $emailError, 'errMSG' => $errMSG));
});
        

$app->post('/registeruser', function ($request, $response, $args) {
    $this->logger->addInfo("registration submitted");

    $data = $request->getParsedBody();

    $name = filter_var($data['name'], FILTER_SANITIZE_STRING);
    $email = filter_var($data['email'], FILTER_SANITIZE_STRING);
    $pass = filter_var($data['pass'], FILTER_SANITIZE_STRING);

    //$this->logger->addInfo($name . $email . $pass);

  // basic name validation
    if (empty($name)) {
        $error = true;     
        $nameError = "Please enter your full name.";
    } 
  
  //basic email validation
    if ( !filter_var($email,FILTER_VALIDATE_EMAIL) ) {
        $error = true;
        $emailError = "Please enter valid email address.";
    } else {
       // check email exist or not
        // Create connection
        $conn = new mysqli('isr-sqlpika.isr.umich.edu', 'eftuser', 'eftuser', 'eft');

        // Check connection
        if ($conn->connect_error) {
            die("Connection failed: " . $conn->connect_error);
        } 

        $sql = "SELECT userEmail FROM users WHERE userEmail='$email'";
        if(!$result = $conn->query($sql)){
            die('There was an error running the query [' . $conn->error . ']');
        }        
        $row = $result->fetch_assoc();

        if ($row['userEmail']==$email) {
            $error = true;
            $emailError = "Provided Email is already in use.";
        }
        mysqli_close($conn);
    }
  // password validation
    if (empty($pass)){
        $error = true;
        $passError = "Please enter password.";
    }
  
    // password encrypt using SHA256();
    $pass = hash('sha256', $pass);
  
    // if there's no error, continue to signup
    if( !$error ) {   
        // Create connection
        $conn = new mysqli('isr-sqlpika.isr.umich.edu', 'eftuser', 'eftuser', 'eft');

        // Check connection
        if ($conn->connect_error) {
            die("Connection failed: " . $conn->connect_error);
        } 

        $query = "INSERT INTO users (userName,userEmail,userPass) VALUES ('$name','$email','$pass')";
        $res = mysqli_query($conn, $query);
        mysqli_close($conn);

        if ($res) {
            $_SESSION['user'] = $email;
            $errTyp = "success";
            $errMSG = "You were successfully registered!";
            //unset($name);
            //unset($email);
            //unset($pass);
        } else {
            $errTyp = "failed";
            $errMSG = "Something went wrong, try again later..."; 
        } 
    }
    return $this->renderer->render($response, 'index.phtml', array('nameError' => $nameError, 'passError' => $passError, 'emailError' => $emailError, 'errTyp' => $errTyp, 'errMSG' => $errMSG));
});
    
$app->post('/processuserinput_maps/{page}', function ($request, $response, $args) {
    
    $page_id = $args['page'];
    //$this->logger->addInfo("user submitted form from: " . $page_id);

    $data = $request->getParsedBody();
    
    // user clicked analysis button
    if( isset($data['StartAnalysis_maps']) ) {
        $this->logger->addInfo("start analysis maps was clicked");
        $buttonpushed = 'yes';
        $post_data = [];
        
        $post_data['mergeindex_maps'] = filter_var($data['mergeindex_maps'], FILTER_SANITIZE_STRING);
        $post_data['candidates_maps'] = implode(',', $data['candidates_maps']);
        $post_data['methods_maps'] = implode(',', $data['methods_maps']);

        //$post_data["userSelectedFile"] = ($_FILES['userfile']['name'] == "") ? $_POST['country'] : $_FILES['userfile']['name'];	

        $analysis_data_maps = new ClusterMapsPostData($post_data);
        $_SESSION["analysis_data_maps"] = serialize($analysis_data_maps); 

        $maps = new ClusterMaps();
        $images = $maps->doAnalysis($analysis_data_maps, $this->logger);
        $_SESSION["map_images"] = $images;
    } 
    else {
        // user selected country dropdown or uploaded file
        if( $data['country_maps'] != "" ) {            
            //session_unset();
unset($_SESSION['CSV_data']);
unset($_SESSION['userSelectedFile']);
unset($_SESSION['filesWereUploaded_maps']);
unset($_SESSION['mi_lines']);
unset($_SESSION['can_lines']);
unset($_SESSION['array']);
unset($_SESSION['head']);
unset($_SESSION['map_images']);
unset($_SESSION['analysis_data_maps']);
unset($_SESSION['userfileText_maps']);
unset($_SESSION['userSelectedFile_maps']);
unset($_SESSION['analysis_data']);
unset($_SESSION['country_maps']);


            $_SESSION["filesWereUploaded_maps"] = $data['country_maps'];
            $this->logger->addInfo($_SESSION["filesWereUploaded_maps"]);
        }
        else if (isset($_FILES['userfile_maps']['name'][0])) {
            //session_unset();
unset($_SESSION['CSV_data']);
unset($_SESSION['userSelectedFile']);
unset($_SESSION['filesWereUploaded_maps']);
unset($_SESSION['mi_lines']);
unset($_SESSION['can_lines']);
unset($_SESSION['array']);
unset($_SESSION['head']);
unset($_SESSION['map_images']);
unset($_SESSION['analysis_data_maps']);
unset($_SESSION['userfileText_maps']);
unset($_SESSION['userSelectedFile_maps']);
unset($_SESSION['analysis_data']);
unset($_SESSION['country_maps']);


            $sessionid = session_id();
            $_SESSION["filesWereUploaded_maps"] = "yes";

            $map_files = array();
            // Count # of uploaded files in array
            $total = count($_FILES['userfile_maps']['name']);
            //$this->logger->addInfo("count: " .  $total);
            // Loop through each file
            for($i=0; $i<$total; $i++) {
                //Get the temp file path                  
                $mapFileName = $sessionid . $_FILES['userfile_maps']['name'][$i];
                $this->logger->addInfo($mapFileName);

                $tmpFilePath = $_FILES['userfile_maps']['tmp_name'][$i];

                //Make sure we have a filepath
                if ($tmpFilePath != ""){
                    //Setup our new file path
                    $uploads_dir = __DIR__ . "/../public/Uploads/";
                    $newFilePath = $uploads_dir . $mapFileName;
                    move_uploaded_file($tmpFilePath, $newFilePath);
                    $map_files[] = $mapFileName;
                }
            }
        }

        if ($_SESSION["filesWereUploaded_maps"] != "")
        {
            //session_unset(); dont do this here
            $this->logger->addInfo("file was selected or uploaded");

            //modify to pass filenames uploaded by user or country dropdown selected
            $clusterMaps = new ClusterMaps();
            $clusterMaps->loadData($map_files);

            // save filename and show on page, also show input panel
            $_SESSION["userSelectedFile_maps"] = ($_FILES['userfile_maps']['name'][0] == "") ? $data['country_maps'] : $_FILES['userfile_maps']['name'][0];	
        }
    }
    // Render index view
    //return $this->renderer->render($response, '/' . $page_id . '.phtml', $args);

    $response = $response->withRedirect('/' . $page_id);
    return $response;
});



$app->post('/processuserinput/{page}', function ($request, $response, $args) {
    
    $page_id = $args['page'];
    //$this->logger->addInfo("user submitted form from: " . $page_id);

    $data = $request->getParsedBody();
    
    // user clicked analysis button
    if( isset($data['StartAnalysis']) ) {
        $this->logger->addInfo("start analysis was clicked");
        $buttonpushed = 'yes';
        $post_data = [];
        
        $post_data['candidates'] = implode(',', $data['candidates']);
        $post_data['level'] = filter_var($data['level'], FILTER_SANITIZE_STRING);
        $post_data['registered'] = filter_var($data['registered'], FILTER_SANITIZE_STRING);
        $post_data['votes'] = filter_var($data['votes'], FILTER_SANITIZE_STRING);
        $post_data['methods'] = implode(',', $data['methods']);

        $post_data["userSelectedFile"] = ($_FILES['userfile']['name'] == "") ? $_POST['country'] : $_FILES['userfile']['name'];	

        $analysis_data = new AnalysisPostData($post_data);
        $_SESSION["analysis_data"] = serialize($analysis_data); 

        $bfa = new BasicForensicsAnalysis();
        $bfa->doAnalysis($analysis_data, $this->logger);
    } 
    else {
        // user selected country dropdown or uploaded file
        if( isset($data['country']) || isset($_FILES['userfile']['name']) ) { 
            //session_unset();
unset($_SESSION['CSV_data']);
unset($_SESSION['userSelectedFile']);
unset($_SESSION['filesWereUploaded_maps']);
unset($_SESSION['mi_lines']);
unset($_SESSION['can_lines']);
unset($_SESSION['array']);
unset($_SESSION['head']);
unset($_SESSION['map_images']);
unset($_SESSION['analysis_data_maps']);
unset($_SESSION['userfileText_maps']);
unset($_SESSION['userSelectedFile_maps']);
unset($_SESSION['analysis_data']);
unset($_SESSION['country_maps']);

            $this->logger->addInfo("file was selected: " . $_FILES['userfile']['name']);
 
            // save filename and show on page, also show input panel
            $_SESSION["userSelectedFile"] = ($_FILES['userfile']['name'] == "") ? $data['country'] : $_FILES['userfile']['name'];	

            // get data from file and save in session variables
            $fileManager = new FileManager();
            $fileManager->getFileContents($data['country'],$this->logger);
        }
    }
    // Render index view
    //return $this->renderer->render($response, '/' . $page_id . '.phtml', $args);

    $response = $response->withRedirect('/' . $page_id);
    return $response;
});


$app->get('/downloadFiles/{country}/{type}', function ($request, $response, $args) {

    $country = $args['country'];
    $type = $args['type'];

    $this->logger->addInfo("downloading data files");
    //$this->logger->addInfo($country);
    //$this->logger->addInfo($type);
    
    $dir = "Data/". $country . "/" .$type . "/";

    $this->logger->addInfo($dir);

    if (is_dir($dir) ) {
        $this->logger->addInfo('valid');
        //$files = scandir($dir, 0);
        //$this->logger->addInfo(print_r($files));

        $zipname = $country . '_' . $type . '.zip';
        $zip = new ZipArchive;
        $zip->open($zipname, ZipArchive::CREATE);

        $files = array();
        foreach (new DirectoryIterator($dir) as $fileInfo) {
            if($fileInfo->isDot() || !$fileInfo->isFile()) continue;
            //$this->logger->addInfo('f-'.$fileInfo->getFilename());
            ////$files[] = $fileInfo->getFilename();
            $zip->addFile($dir . $fileInfo->getFilename());
        }
        //$zip->addFile($dir . "2014_afghanistan_election_results.csv");
        //$zip->addFile($dir . "2014_afghanistan_preliminary_runoff_election_results.csv");
        //$zip->addFile('sub/test.html');
        //$zip->addFile('sub/readme.txt');
        
        $zip->close();
    }
    else {
        $this->logger->addInfo('not valid directory');
    }
    readfile($zipname);

    header('Content-Description: File Transfer');
    header('Content-Type: application/zip');
    header('Content-Disposition: attachment; filename="'.basename($zipname).'"');
    //header('Expires: 0');
    //header('Cache-Control: must-revalidate');
    //header('Pragma: public');
    header('Content-Length: ' . filesize($zipname));
    readfile($zipname);
    exit;
});


$app->get('/download', function ($request, $response, $args) {
    $this->logger->addInfo("downloading csv results file");

    $file = __DIR__ . "/../public/Results/results" . session_id() . ".csv";
    header('Content-Description: File Transfer');
    header('Content-Type: application/octet-stream');
    header('Content-Disposition: attachment; filename="'.basename($file).'"');
    header('Expires: 0');
    header('Cache-Control: must-revalidate');
    header('Pragma: public');
    header('Content-Length: ' . filesize($file));
    readfile($file);
    exit;
});

$app->get('/downloadHTML', function ($request, $response, $args) {
    $this->logger->addInfo("downloading html results file");
    $file = __DIR__ . "/../public/Results/results" . session_id() . ".html";
    header('Content-Description: File Transfer');
    header('Content-Type: application/octet-stream');
    header('Content-Disposition: attachment; filename="'.basename($file).'"');
    header('Expires: 0');
    header('Cache-Control: must-revalidate');
    header('Pragma: public');
    header('Content-Length: ' . filesize($file));
    readfile($file);
    exit;
});


$app->get('/downloadMaps', function ($request, $response, $args) {
    $this->logger->addInfo("downloading maps results file");
    $file = __DIR__ . "/../public/Results/results" . session_id() . ".pdf";
    header('Content-Description: File Transfer');
    header('Content-Type: application/octet-stream');
    header('Content-Disposition: attachment; filename="'.basename($file).'"');
    header('Expires: 0');
    header('Cache-Control: must-revalidate');
    header('Pragma: public');
    header('Content-Length: ' . filesize($file));
    readfile($file);
    exit;
});




$app->get('/hello/{name}', function ($request, $response, $args) {
    $name = $request->getAttribute('name');
    $response->getBody()->write("Hello, $name");

    $this->logger->addInfo("Something interesting happened");

    //return $response;
    return $this->renderer->render($response, 'index - Copy.phtml', $args);
});


$app->get('/', function ($request, $response, $args) {
    $this->logger->addInfo("home page");

    // Render index view
    return $this->renderer->render($response, 'index.phtml', $args);
});

$app->get('/about', function ($request, $response, $args) {
    return $this->renderer->render($response, 'about.phtml', $args);
});

$app->get('/analysis', function ($request, $response, $args) {
    return $this->renderer->render($response, 'analysis.phtml', $args);
});

$app->get('/clustermaps', function ($request, $response, $args) {
    return $this->renderer->render($response, 'clustermaps.phtml', $args);
});

$app->get('/datasummary', function ($request, $response, $args) {
    return $this->renderer->render($response, 'datasummary.phtml', $args);
});

$app->get('/displaydata', function ($request, $response, $args) {
    return $this->renderer->render($response, 'displaydata.phtml', $args);
});


$app->get('/help', function ($request, $response, $args) {
    return $this->renderer->render($response, 'help.phtml', $args);
});





