C$Program     COMMNT ( Commenting SPK and CK files )
 
      PROGRAM COMMNT
 
C$ Abstract
C
C     NAIF Toolkit utility program for adding, extracting, reading
C     and deleting comments from a binary SPK or CK file.
C
C$ Required_Reading
C
C     SPC
C
C$ Author_and_Institution
C
C     J.E. McLean    (JPL)
C
C$ Version
C
C-    Version 3.0.1, 10-AUG-1991 (CHA) (NJB)
C
C        Updated comments to reflect status as a Toolkit
C        utility program.  Message indicating that no comments
C        were found in the specified file was changed to include
C        the file name.
C
C-    Version 2.0.0, 28-JUN-1991 (JEM)
C
C        The option to read the comments from the comment
C        area of a binary SPK or CK was added to the menu.
C
C-    Version 1.0.0, 05-APR-1991 (JEM)
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER               RTRIM
 
 
C
C     Variables
C
      CHARACTER*(80)        BMARK
      CHARACTER*(128)       COMFIL
      CHARACTER*(80)        EMARK
      CHARACTER*(128)       LINE
      CHARACTER*(80)        OPTION
      CHARACTER*(200)       OUTSTR
      CHARACTER*(128)       SPCFIL
 
      INTEGER               HANDLE
      INTEGER               UNIT
 
      LOGICAL               EOC
      LOGICAL               QUIT
 
      CALL CHKIN ( 'COMMNT' )
 
50001 IF       ( .NOT. QUIT )
     .THEN
 
         WRITE (*,*)
         WRITE (*,*) 'Options:'
         WRITE (*,*)
         WRITE (*,*) '(1) Quit'
         WRITE (*,*) '(2) Add comments'
         WRITE (*,*) '(3) Extract comments'
         WRITE (*,*) '(4) Read comments'
         WRITE (*,*) '(5) Delete comments'
         WRITE (*,*)
 
         WRITE (*,*) 'Enter an option:'
         READ  (*,FMT='(A)') OPTION
         WRITE (*,*)
 
         IF ( OPTION .EQ. '1' ) THEN
 
            QUIT = .TRUE.
 
         ELSE IF ( OPTION .EQ. '2' ) THEN
 
C
C           Get the SPK or CK (DAF) file name and open it for
C           write access
C
            WRITE (*,*)
            WRITE (*,*) 'Name of a binary SPK or CK file'
            READ  (*,FMT='(A)') SPCFIL
            WRITE (*,*)
 
            CALL DAFOPW ( SPCFIL, HANDLE )
 
C
C           Get the comment file name and open it for read access.
C
            WRITE (*,*) 'Name of text file that contains comments?'
            READ  (*,FMT='(A)') COMFIL
            WRITE (*,*)
 
            CALL TXTOPR ( COMFIL, UNIT )
 
C
C           Get the markers that delimit the group of lines in the
C           comment file that are to be added to the comment area
C           of the SPK or CK file.  Call SPCAC to add the comments.
C
C           If BMARK is a blank string, SPCAC assumes that the comments
C           begin on the first line of the comment file.  Similarly,
C           if EMARK is a blank string, SPCAC assumes that the comments
C           end on the last line of the comment file.
C
            WRITE (*,*) 'Begin marker?'
            WRITE (*,*) '(Just hit <Return> if comments begin on '    //
     .                  'the first line of the file)'
            READ  (*,FMT='(A)') BMARK
            WRITE (*,*)
 
            WRITE (*,*) 'End marker?'
            WRITE (*,*) '(Just hit <Return> if comments end on '      //
     .                  'the last line of the file)'
            READ  (*,FMT='(A)') EMARK
            WRITE (*,*)
 
            WRITE (*,*) 'Adding comments to comment area of binary '  //
     .                  'file...'
            WRITE (*,*)
 
            CALL SPCAC  ( HANDLE, UNIT, BMARK, EMARK )
 
C
C           Close both files.
C
            CALL DAFCLS ( HANDLE )
            CLOSE ( UNIT )
 
         ELSE IF ( OPTION .EQ. '3' ) THEN
 
C
C           Get the SPK or CK (DAF) file name and open it for
C           read access
C
            WRITE (*,*)
            WRITE (*,*) 'Name of a binary SPK or CK file?'
            READ  (*,FMT='(A)') SPCFIL
            WRITE (*,*)
 
            CALL DAFOPR ( SPCFIL, HANDLE )
 
C
C           Get the name and open the new text file to which the
C           comments are to be written.
C
            WRITE (*,*) 'Name of text file to be created?'
            READ  (*,FMT='(A)') COMFIL
            WRITE (*,*)
 
            CALL TXTOPN ( COMFIL, UNIT )
 
C
C           SPCEC extracts whatever comments are in the comment area
C           of the SPK or CK file and writes them to the text file.
C
            WRITE (*,*) 'Extracting comments from comment area '      //
     .                  'of binary file'
            WRITE (*,*) 'and writing to text file...'
            WRITE (*,*)
 
            CALL SPCEC ( HANDLE, UNIT )
 
C
C           Close both files.
C
            CALL DAFCLS ( HANDLE )
            CLOSE ( UNIT )
 
 
         ELSE IF ( OPTION .EQ. '4' ) THEN
 
C
C           Get the name of the file and open it for read
C           access.
C
            WRITE (*,*)
            WRITE (*,*) 'Name of a binary SPK or CK file?'
            READ  (*,FMT='(A)') SPCFIL
            WRITE (*,*)
            WRITE (*,*)
 
            CALL DAFOPR ( SPCFIL, HANDLE )
 
C
C           Read lines of comments and write them to the screen.
C
            CALL SPCRFL ( HANDLE, LINE, EOC )
 
            IF ( EOC ) THEN
 
               OUTSTR    =    SPCFIL
               CALL PREFIX ( 'No comments found in file', 1, OUTSTR )
               WRITE (*,*)    OUTSTR
 
            END IF
 
50002       IF       ( .NOT. EOC )
     .      THEN
 
               WRITE (*,*) LINE ( 1: RTRIM(LINE) )
 
               CALL SPCRNL ( LINE, EOC )
 
               GO TO 50002
            END IF
 
            WRITE (*,*)
 
C
C           Close the file.
C
            CALL DAFCLS ( HANDLE )
 
         ELSE IF ( OPTION .EQ. '5' ) THEN
 
C
C           Get the name of the SPK or CK (DAF) file and open it for
C           write access.
C
            WRITE (*,*)
            WRITE (*,*) 'Name of a binary SPK or CK file?'
            READ  (*,FMT='(A)') SPCFIL
            WRITE (*,*)
 
            CALL DAFOPW ( SPCFIL, HANDLE )
 
C
C           Delete the comments.
C
            WRITE (*,*) 'Deleting comments from comment area of '     //
     .                  'binary file...'
            WRITE (*,*)
 
            CALL SPCDC  ( HANDLE )
 
C
C           Close the file.
C
            CALL DAFCLS ( HANDLE )
 
         ELSE
 
            WRITE (*,*) 'Invalid option, try again . . .'
            WRITE (*,*)
 
         END IF
 
         GO TO 50001
      END IF
 
      CALL CHKOUT ( 'COMMNT' )
 
      END
