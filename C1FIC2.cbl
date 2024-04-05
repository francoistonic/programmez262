       IDENTIFICATION DIVISION.
      * 1ERE          DIVISION
       PROGRAM-ID. C1FICHO.

      *****************************************************
      *      EXEMPLE DE PROGRAMME
      *      AVEC DES OUVERTURES DIFFERENTES
      *      - OUTPUT SUIVI DE READ
      *      - OUTPUT SUIVI DE CLOSE
      *      - EXTEND POUR ECRIRE A LA SUITE
      *      CHAQUE OUVERTURE EST CONDITIONNESE
      *      PAR UN PARAMETRE
      *
      *****************************************************
      *      REMARQUE : AVEC L'OPTION EXTEND
      *         ON CONNAIT LE NOMBRE D'ENREGISTREMENTS AJOUTES
      *         ON IGNORE LE NOMBRE TOTAL D'ENREGISTREMENTS
      *
      *****************************************************

       ENVIRONMENT DIVISION.
      * 2EME          DIVISION
      *  LES PHRASES SE TERMINENT PAR UN POINT
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               SELECT FIN-ATRIER ASSIGN TO INATRIER
               FILE STATUS IS WS-FS-FIN-ATRIER.
      *
               SELECT FOU-TRIE ASSIGN TO OUTRIE
               FILE STATUS IS WS-FS-FOU-TRIE.
       DATA DIVISION.
      * 3EME          DIVISION
       FILE SECTION .
       FD FIN-ATRIER
            RECORDING MODE IS F.
       01  FS-IN-ATRIER    PIC X(50).
      *
       FD FOU-TRIE
            RECORDING MODE IS F.
       01  FS-OU-TRIE     PIC X(50).
      *
       WORKING-STORAGE SECTION.
      * SECTION DES DONNEES DE  TRAVAIL
      *************************************************
      * ZONES D'ENREGISTREMENT
      *************************************************
       01  WS-IN-ATRIER    .
           05   WS-IN-NOMBRE   PIC 999.
           05   FILLER         PIC X(78).
       01  WS-OU-TRIE    .
           05   WS-OU-NOMBRE   PIC 999.
           05   FILLER         PIC X(78).
      *************************************************
      * STATUS
      *************************************************
       01    WS-FS-FIN-ATRIER PIC XX.
       01    WS-FS-FOU-TRIE   PIC XX.
      *************************************************
      * ZONES DE CALCULS
      *************************************************
       01 WS-MESSAGE  .
          05 WS-MESSAGE-OUV PIC X(10).
          05 FILLER         PIC X(70).
      *****************************************************
                                                                        00028800
      ********************************************************          00028900
      * ZONES DE TRAVAIL                                                00029000
      ********************************************************          00029100
                                                                        00029200
       01 WS-FNOMB-FLAG     PIC  XXX   VALUE SPACE.                     00029300
       01 WS-CTR-FIN-ATRIER PIC 9999 VALUE ZERO .                       00029400
       01 WS-CTR-FOU-TRIE   PIC 9999 VALUE ZERO .                       00029400
      *
       01 WS-TVAL.                                                      00029500
          05 FILLER                  OCCURS 21.                         00029600
             10 WS-NOMBRE PIC  999            .                         00029700
      *
       01 WS-NOMBRE-POSTE PIC 99 VALUE 21 .                             00029800
       01 WS-IND          PIC   99            .                         00029900
       01 WS-TEMP         PIC  999            .                         00030000
       01 WS-CTR1         PIC   99            .                         00030100
       01 WS-CTR1DIV2     PIC   99            .                         00030200
       01 WS-CTR2         PIC   99            .                         00030300
       01 WS-RESTE        PIC    9            .                         00030400
                                                                        00030500
      ********************************************************          00030600
       PROCEDURE DIVISION.                                              00030700
                                                                        00030800
       0000-LECTURE-DEB.                                                00030900
      * OUVRIR UN FICHIER                                               00031000
           ACCEPT WS-MESSAGE.
           DISPLAY '*****************************'
           DISPLAY 'LA DEMANDE EST : ' WS-MESSAGE.
           DISPLAY '*****************************'
           EVALUATE      WS-MESSAGE-OUV
              WHEN   'A LA SUITE'
                 PERFORM 6130-TRIE-OUV-EXTEND-DEB                       00031100
                    THRU 6130-TRIE-OUV-EXTEND-FIN                       00031200
              WHEN   'VIDER'
                 PERFORM 6100-TRIE-OUV-DEB                              00031100
                   THRU  6100-TRIE-OUV-FIN                              00031200
                 MOVE 'FIN' TO       WS-FNOMB-FLAG

              WHEN OTHER
                 PERFORM 6100-TRIE-OUV-DEB                              00031100
                    THRU 6100-TRIE-OUV-FIN                              00031200
           END-EVALUATE.
                                                                        00031300
           PERFORM 6000-ATRIER-OUV-DEB                                  00031100
              THRU 6000-ATRIER-OUV-FIN.                                 00031200
      * LIRE LE FICHIER                                                 00031400
           PERFORM 6010-ATRIER-LEC-DEB                                  00031500
           THRU    6010-ATRIER-LEC-FIN.                                 00031600
      * SI FIN -> ANOMALIE                                              00031700
           IF WS-FNOMB-FLAG = 'FIN'                                     00031800
              DISPLAY 'ANOMALIE : FICHIER VIDE'                         00031900
              PERFORM 9998-FIN-NORMALE-DEB                              00034400
                 THRU 9998-FIN-NORMALE-FIN.                             00034500
                                                                        00032100
      * SINON WS-IN-NOMBRE = ENREGISTREMENT                             00032200
           PERFORM 1000-CHARG-DEB                                       00032300
             THRU  1000-CHARG-FIN                                       00032400
             UNTIL   WS-FNOMB-FLAG = 'FIN'.                             00032500
                                                                        00032600
      * TRAITEMENT PRINCIPAL                                            00032700
           DISPLAY WS-TVAL.                                             00032800
           PERFORM 1010-INIT-DEB                                        00032900
             THRU  1010-INIT-FIN.                                       00033000
           PERFORM 1020-AFFICH-DEB                                      00033100
             THRU  1020-AFFICH-FIN                                      00033200
             VARYING WS-IND FROM WS-IND BY -1                           00033300
             UNTIL   WS-IND < 1.                                        00033400
                                                                        00033500
      * FERMETURE FICHIER                                               00033600
           PERFORM 6020-ATRIER-CLO-DEB                                  00033700
              THRU 6020-ATRIER-CLO-FIN.                                 00033800
                                                                        00033900
      * AFFICHAGE DES STATISTIQUES                                      00034000
           PERFORM 8999-STATISTIQUES-DEB                                00034100
              THRU 8999-STATISTIQUES-FIN.                               00034200
                                                                        00034300
           PERFORM 9998-FIN-NORMALE-DEB                                 00034400
              THRU 9998-FIN-NORMALE-FIN.                                00034500
                                                                        00034600
       0000-LECTURE-FIN. EXIT.                                          00034700
                                                                        00034800
       1000-CHARG-DEB.                                                  00034901
      * LIT LES 20 NOMBRES                                              00035000
           MOVE WS-IN-NOMBRE TO WS-NOMBRE (WS-CTR-FIN-ATRIER).          00036000
           PERFORM 6010-ATRIER-LEC-DEB                                  00037000
           THRU    6010-ATRIER-LEC-FIN.                                 00038000
       1000-CHARG-FIN. EXIT.                                            00039000
                                                                        00040000
       1010-INIT-DEB.                                                   00050000
      * REMISE DE WS-IND … 'NOMBRE DE POSTE'                            00060000
           MOVE WS-NOMBRE-POSTE TO WS-IND.                              00070000
           PERFORM 7000-TRI-COCKTAIL-DEB                                00071000
           THRU    7000-TRI-COCKTAIL-FIN.                               00072000
       1010-INIT-FIN. EXIT.                                             00073000
                                                                        00074000
       1020-AFFICH-DEB.                                                 00074100
      * AFFICHE LE NOMBRE CONTENU DANS UNE LIGNE DE LA TABLE            00074200
           DISPLAY 'LA LIGNE ' WS-IND ' DE LA TABLE TVAL'               00074300
                   ' CONTIENT LA VALEUR ' WS-NOMBRE(WS-IND).            00074400
      * ECRITURE DANS LE FICHIER TRIE
           MOVE SPACE               TO WS-OU-TRIE.
           MOVE WS-NOMBRE(WS-IND)   TO WS-OU-NOMBRE.
           PERFORM 6110-TRIE-ECR-DEB
            THRU   6110-TRIE-ECR-FIN.

       1020-AFFICH-FIN. EXIT.                                           00074500
                                                                        00074600
      *********************************************************         00074700
                                                                        00074800
       6000-ATRIER-OUV-DEB.                                             00074900
           OPEN INPUT FIN-ATRIER.                                       00075000
           IF WS-FS-FIN-ATRIER NOT = ZERO                               00075100
              DISPLAY "ERREUR OPEN ATRIER "                             00075100
              PERFORM 9999-ERREUR-PROGRAMME-DEB                         00075100
                THRU  9999-ERREUR-PROGRAMME-FIN                         00075100
           END-IF.                                                      00075100
       6000-ATRIER-OUV-FIN.                                             00075200
           EXIT.                                                        00075300
                                                                        00075400
       6010-ATRIER-LEC-DEB.                                             00075500
           READ FIN-ATRIER                                              00075900
                INTO WS-IN-ATRIER
               AT END MOVE 'FIN' TO WS-FNOMB-FLAG.                      00076000
           IF WS-FS-FIN-ATRIER NOT = ZERO AND NOT = '10'                00075100
              DISPLAY "ERREUR READ ATRIER "                             00075100
              PERFORM 9999-ERREUR-PROGRAMME-DEB                         00075100
                THRU  9999-ERREUR-PROGRAMME-FIN                         00075100
           END-IF.                                                      00075100
           IF WS-FS-FIN-ATRIER = '10'                                   00075100
              MOVE 'FIN' TO WS-FNOMB-FLAG                               00075800
           END-IF.                                                      00075100
           IF WS-FS-FIN-ATRIER = ZERO                                   00075100
               DISPLAY FS-IN-ATRIER
               ADD 1 TO WS-CTR-FIN-ATRIER.                              00076200
           IF WS-CTR-FIN-ATRIER > WS-NOMBRE-POSTE
              DISPLAY 'DIMENSION D LA TABLE ATTEINTE '
              DISPLAY 'ENREGISTREMENTS LUS  : ' WS-CTR-FIN-ATRIER
              DISPLAY 'POSTES DANS LA TABLE : ' WS-NOMBRE-POSTE
              MOVE 'FIN' TO WS-FNOMB-FLAG                               00075800
           END-IF.                                                      00075100

       6010-ATRIER-LEC-FIN.                                             00076300
           EXIT.                                                        00076400
                                                                        00076500
       6020-ATRIER-CLO-DEB.                                             00076600
           CLOSE FIN-ATRIER.                                            00076700
           IF WS-FS-FIN-ATRIER NOT = ZERO                               00075100
              DISPLAY "ERREUR CLOSE ATRIER "                            00075100
              PERFORM 9999-ERREUR-PROGRAMME-DEB                         00075100
                THRU  9999-ERREUR-PROGRAMME-FIN                         00075100
           END-IF.                                                      00075100
       6020-ATRIER-CLO-FIN.                                             00076900
           EXIT.                                                        00077000
                                                                        00077100
      *********************************************************         00077200
       6100-TRIE-OUV-DEB.                                               00074900
           OPEN OUTPUT FOU-TRIE.                                        00075000
           IF WS-FS-FOU-TRIE NOT = ZERO                                 00075100
              DISPLAY "ERREUR OPEN OUTPUT TRIE "                        00075100
              PERFORM 9999-ERREUR-PROGRAMME-DEB                         00075100
                THRU  9999-ERREUR-PROGRAMME-FIN                         00075100
           END-IF.                                                      00075100
       6100-TRIE-OUV-FIN.                                               00075200
           EXIT.                                                        00075300
                                                                        00075400
       6110-TRIE-ECR-DEB.                                               00075500
           WRITE  FS-OU-TRIE                                            00075900
                FROM  WS-OU-TRIE.
           IF WS-FS-FOU-TRIE NOT = ZERO                                 00075100
              DISPLAY "ERREUR ECRITURE ATRIER "                         00075100
              PERFORM 9999-ERREUR-PROGRAMME-DEB                         00075100
                THRU  9999-ERREUR-PROGRAMME-FIN                         00075100
           END-IF.                                                      00075100
           IF WS-FS-FOU-TRIE = ZERO                                     00075100
               ADD 1 TO WS-CTR-FOU-TRIE.                                00076200

       6110-TRIE-ECR-FIN.                                               00076300
           EXIT.                                                        00076400
                                                                        00076500
       6120-TRIE-CLO-DEB.                                               00076600
           CLOSE FOU-TRIE.                                              00076700
           IF WS-FS-FOU-TRIE NOT = ZERO                                 00075100
              DISPLAY "ERREUR CLOSE TRIE "                              00075100
              PERFORM 9999-ERREUR-PROGRAMME-DEB                         00075100
                THRU  9999-ERREUR-PROGRAMME-FIN                         00075100
           END-IF.                                                      00075100
       6120-TRIE-CLO-FIN.                                               00076900
           EXIT.                                                        00077000
                                                                        00077100
       6130-TRIE-OUV-EXTEND-DEB.                                        00074900
           OPEN EXTEND  FOU-TRIE.                                       00075000
           IF WS-FS-FOU-TRIE NOT = ZERO                                 00075100
              DISPLAY "ERREUR OPEN EXTEND TRIE "                        00075100
              PERFORM 9999-ERREUR-PROGRAMME-DEB                         00075100
                THRU  9999-ERREUR-PROGRAMME-FIN                         00075100
           END-IF.                                                      00075100
       6130-TRIE-OUV-EXTEND-FIN.                                        00075200
           EXIT.                                                        00075300
      *********************************************************         00077200
       7000-TRI-COCKTAIL-DEB.                                           00077300
      * DEBUT DU TRI COCKTAIL                                           00077400
           MOVE 1 TO WS-CTR1.                                           00077500
           MOVE 0 TO WS-CTR2.                                           00077600
           PERFORM 7010-PARITE-DEB                                      00077700
           THRU    7010-PARITE-FIN                                      00077800
           VARYING WS-CTR1   FROM WS-CTR1 BY 1                          00077900
           UNTIL   WS-CTR1   > WS-NOMBRE-POSTE.                         00078000
       7000-TRI-COCKTAIL-FIN. EXIT.                                     00078100
                                                                        00078200
       7010-PARITE-DEB.                                                 00078300
      * DETERMINE SI CTR1 EST IMPAIR OU PAIR                            00078400
           DIVIDE    WS-CTR1 BY 2                                       00078500
              GIVING    WS-CTR1DIV2                                     00078600
              REMAINDER WS-RESTE.                                       00078700
           IF WS-RESTE NOT = 0                                          00078800
              PERFORM 7020-INC-CTR2-DEB                                 00079000
              THRU    7020-INC-CTR2-FIN                                 00080000
           ELSE                                                         00081000
              PERFORM 7025-DEC-CTR2-DEB                                 00082000
              THRU    7025-DEC-CTR2-FIN                                 00083000
           END-IF.                                                      00084000
       7010-PARITE-FIN. EXIT.                                           00085000
                                                                        00086000
       7020-INC-CTR2-DEB.                                               00087000
      * INCREMENTE CTR2 DE 1                                            00088000
           ADD 1 TO WS-CTR2.                                            00089000
           PERFORM 7030-COMP-DEB                                        00089100
             THRU  7030-COMP-FIN                                        00089200
             VARYING WS-CTR2 FROM WS-CTR2 BY 1                          00089300
             UNTIL   WS-CTR2 >
                  WS-NOMBRE-POSTE - (WS-CTR1DIV2 + 1).
       7020-INC-CTR2-FIN. EXIT.                                         00089500
                                                                        00089600
       7030-COMP-DEB.                                                   00089700
      * COMPARAISON DE 2 POSTES SUCCESSIFS                              00089800
           IF WS-NOMBRE(WS-CTR2) > WS-NOMBRE(WS-CTR2 + 1)               00089900
              PERFORM 7040-PERMUT-DEB                                   00090000
                 THRU 7040-PERMUT-FIN.                                  00090100
       7030-COMP-FIN. EXIT.                                             00090200
                                                                        00090300
       7040-PERMUT-DEB.                                                 00090400
      * PERMUTE LES VALEURS DE 2 POSTES CONSECUTIFS                     00090500
           MOVE WS-NOMBRE(WS-CTR2)                                      00090600
                  TO WS-TEMP.                                           00090700
           MOVE WS-NOMBRE(WS-CTR2 + 1)                                  00090800
                  TO WS-NOMBRE(WS-CTR2).                                00090900
           MOVE WS-TEMP                                                 00091000
                  TO WS-NOMBRE(WS-CTR2 + 1).                            00091100
       7040-PERMUT-FIN. EXIT.                                           00091200
                                                                        00091300
                                                                        00091400
                                                                        00091500
                                                                        00091600
       7025-DEC-CTR2-DEB.                                               00091700
      * DECREMENTE CTR2 DE 1                                            00091800
           SUBTRACT 1 FROM WS-CTR2.                                     00091900
           PERFORM 7035-COMP-DEB                                        00092000
             THRU  7035-COMP-FIN                                        00092100
             VARYING WS-CTR2 FROM WS-CTR2 BY -1                         00092200
             UNTIL   WS-CTR2 < WS-CTR1DIV2 + 1.                         00092300
       7025-DEC-CTR2-FIN. EXIT.                                         00092400
                                                                        00092500
       7035-COMP-DEB.                                                   00092600
      * COMPARAISON DE 2 POSTES SUCCESSIFS                              00092700
           IF WS-NOMBRE(WS-CTR2) < WS-NOMBRE(WS-CTR2 - 1)               00092800
              PERFORM 7045-PERMUT-DEB                                   00092900
                 THRU 7045-PERMUT-FIN.                                  00093000
       7035-COMP-FIN. EXIT.                                             00093100
                                                                        00093200
       7045-PERMUT-DEB.                                                 00093300
      * PERMUTE LES VALEURS DE 2 POSTES CONSECUTIFS                     00093400
           MOVE WS-NOMBRE(WS-CTR2)                                      00093500
                  TO WS-TEMP.                                           00093600
           MOVE WS-NOMBRE(WS-CTR2 - 1)                                  00093700
                  TO WS-NOMBRE(WS-CTR2).                                00093800
           MOVE WS-TEMP                                                 00093900
                  TO WS-NOMBRE(WS-CTR2 - 1).                            00094000
       7045-PERMUT-FIN. EXIT.                                           00094100
                                                                        00094200
                                                                        00094300
      *********************************************************         00094400
       8999-STATISTIQUES-DEB.                                           00096900
           DISPLAY ' '.                                                 00097000
           DISPLAY '**********************************'                 00097100
           DISPLAY '* STATISTIQUES DU PROGRAMME C1TRI3*'                00097200
           DISPLAY '* XXXXXXXXXXXXXXXXXXXXXXXXX *'                      00097300
           DISPLAY '**********************************'                 00097400
           DISPLAY '                                  '                 00097500
           DISPLAY 'ENREGISTREMENTS LUS   ' WS-CTR-FIN-ATRIER.          00097600
           DISPLAY 'ENREGISTREMENTS ECRITS' WS-CTR-FOU-TRIE.            00097600
           DISPLAY ' '.                                                 00097700
       8999-STATISTIQUES-FIN. EXIT.                                     00097800
                                                                        00097900
      *********************************************************         00098000
       9998-FIN-NORMALE-DEB.                                            00098100
                                                                        00098200
           DISPLAY '**********************************'                 00098300
           DISPLAY '* FIN NORMALE DU PROGRAMME C1TRI3 *'                00098400
           DISPLAY '**********************************'                 00098600
                   ' '.                                                 00098700
           STOP RUN.                                                    00098800
                                                                        00098900
       9998-FIN-NORMALE-FIN. EXIT.                                      00099000
                                                                        00099100
       9999-ERREUR-PROGRAMME-DEB.                                       00099200
                                                                        00099300
           DISPLAY '**********************************'                 00099400
           DISPLAY '* UNE ANOMALIE A ETE DETECTEE *'                    00099500
           DISPLAY '* ERREUR DANS LES ENREGISTREMENTS*'                 00099600
           DISPLAY '**********************************'.                00099700
           DISPLAY ' WS-FS-FIN-ATRIER :   ' WS-FS-FIN-ATRIER.           00099700
           DISPLAY ' WS-FS-FOU-TRIE   :   ' WS-FS-FOU-TRIE.             00099700
           DISPLAY 'ENREGISTREMENTS LUS   ' WS-CTR-FIN-ATRIER.          00097600
           DISPLAY 'ENREGISTREMENTS ECRITS' WS-CTR-FOU-TRIE.            00097600
           STOP RUN.                                                    00099800
                                                                        00099900
       9999-ERREUR-PROGRAMME-FIN. EXIT.                                 00100000
