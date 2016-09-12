<?php 


class ClusterMapsPostData
{
    public $mergeindex;
    public $candidates;
    public $methods;
    public $userSelectedFile;
    /**
     * Accept an array of data matching properties of this class
     * and create the class
     *
     * @param array $data The data to use to create
     */
    public function __construct(array $data) {
        $this->mergeindex = $data['mergeindex_maps'];
        $this->candidates = $data['candidates_maps'];
        $this->methods = $data['methods_maps'];
        $this->userSelectedFile = $data['userSelectedFile_maps'];
    }

    public function getMergeIndex() {
        return $this->mergeindex;
    }
    public function getCandidates() {
        return $this->candidates;
    }
    
    public function getMethods() {
        return $this->methods;
    }
    public function getUserSelectedFile() {
        return $this->userSelectedFile;
    }

}