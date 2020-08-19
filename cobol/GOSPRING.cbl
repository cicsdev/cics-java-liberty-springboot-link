       CBL CICS('COBOL3,SP')
      *---------------------------------------------------------------*
      *  Licensed Materials - Property of IBM                         *
      *  SAMPLE                                                       *
      *  (c) Copyright IBM Corp. 2020 All Rights Reserved             *
      *  US Government Users Restricted Rights - Use, duplication or  *
      *  disclosure restricted by GSA ADP Schedule Contract with      *
      *  IBM Corp                                                     *
      *---------------------------------------------------------------*

      *****************************************************************
      * Link to Spring Boot sample driver program                     *
      *                                                               *
      * This program can be used the initial program of a transaction *
      * to call YOSPRING, passing a suitable channel.  YOSPRING will  *
      * invoke a Spring Boot application in a Liberty JVM server.     *
      *****************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. GOSPRING.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MESSAGE-CHAN PIC X(16) VALUE 'CHAN'.
       01 MESSAGE-CONT PIC X(16) VALUE 'MESSAGE'.
       01 MESSAGE-TEXT PIC X(24) VALUE 'Hello Spring Boot World!'.

       LINKAGE SECTION.
       PROCEDURE DIVISION.
      ***************************************************************
      *    Main section                                             *
      ***************************************************************
           EXEC CICS PUT CONTAINER(MESSAGE-CONT)
                         CHANNEL(MESSAGE-CHAN)
                         FROM(MESSAGE-TEXT)
                         CHAR
           END-EXEC.

           EXEC CICS LINK PROGRAM('YOSPRING')
                          CHANNEL(MESSAGE-CHAN)
           END-EXEC.

           EXEC CICS RETURN END-EXEC.

           GOBACK.
