package com.jgp.infrastructure.bulkimport.exception;

public class ImportTypeNotFoundException extends RuntimeException {

    public ImportTypeNotFoundException(){
        super("Import type not found !!");
    }
}
