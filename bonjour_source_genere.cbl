000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.  PBONJO.                                             PBONJO
000030 AUTHOR.         premier programme bonjour.                       PBONJO
000040 DATE-COMPILED.   28/02/00.                                       PBONJO
000050 ENVIRONMENT DIVISION.                                            PBONJO
000060 CONFIGURATION SECTION.                                           PBONJO
000070 SOURCE-COMPUTER. IBM-370.                                        PBONJO
000080 OBJECT-COMPUTER. IBM-370.                                        PBONJO
000090 DATA DIVISION.                                                   PBONJO
000100 WORKING-STORAGE SECTION.                                         PBONJO
000110 01               DEBUT-WSS.                                      PBONJO
000120   05             FILLER        PICTURE X(7) VALUE                PBONJO
000130                                'WORKING'.                        PBONJO
000140   05             IK            PICTURE X.                        PBONJO
000150 01               CONSTANTES-PAC.                                 PBONJO
000160   05             FILLER        PICTURE X(60) VALUE               PBONJO
000170     '0096 LBA28/02/00PBONJOTEST    11:33:18PBONJO  BVAP28/02/2000PBONJO
000180-    ''     .                                                      PBONJO
000190 01               PAC-CONSTANTES REDEFINES CONSTANTES-PAC.        PBONJO
000200   05             NUGNA         PICTURE X(5).                     PBONJO
000210   05             APPLI         PICTURE X(3).                     PBONJO
000220   05             DATGN         PICTURE X(8).                     PBONJO
000230   05             PROGR         PICTURE X(6).                     PBONJO
000240   05             CODUTI        PICTURE X(8).                     PBONJO
000250   05             TIMGN         PICTURE X(8).                     PBONJO
000260   05             PROGE         PICTURE X(8).                     PBONJO
000270   05             COBASE        PICTURE X(4).                     PBONJO
000280   05             DATGNC        PICTURE X(10).                    PBONJO
000290 01               DATCE.                                          PBONJO
000300   05             CENTUR        PICTURE XX VALUE                  PBONJO
000310                                '19'.                             PBONJO
000320   05             DATOR.                                          PBONJO
000330     10           DATOA         PICTURE XX.                       PBONJO
000340     10           DATOM         PICTURE XX.                       PBONJO
000350     10           DATOJ         PICTURE XX.                       PBONJO
000360 01               VARIABLES-CONDITIONNELLES.                      PBONJO
000370   05             FT            PICTURE X VALUE '0'.              PBONJO
000380 PROCEDURE DIVISION.                                              PBONJO
000390 N01.                                                             PBONJO
000400           NOTE *************************************.            PBONJO
000410                *                                   *             PBONJO
000420                *INITIALISATIONS                    *             PBONJO
000430                *                                   *             PBONJO
000440                *************************************.            PBONJO
000450 F01.                                                             PBONJO
000460     EXIT.                                                        PBONJO
000470 F01-FN.                                                          PBONJO
000480     EXIT.                                                        PBONJO
000490 F02.                                                             P100
000500     DISPLAY 'BONJOUR TOUT LE MONDE'                              P100
000510     MOVE ALL '1' TO FT.                                          P120
000520 F02-FN.                                                          P120
000530     EXIT.                                                        P120
000540*          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            PBONJO
000550 F05.                                                             PBONJO
000560     EXIT.                                                        PBONJO
000570 N20.                                                             PBONJO
000580           NOTE *************************************.            PBONJO
000590                *                                   *             PBONJO
000600                *FIN DE TRAITEMENT                  *             PBONJO
000610                *                                   *             PBONJO
000620                *************************************.            PBONJO
000630 F20.                                                             PBONJO
000640     IF      FT  =  ALL '1'                                       PBONJO
000650         NEXT SENTENCE                                            PBONJO
000660     ELSE                                                         PBONJO
000670         GO TO F20-FN.                                            PBONJO
000680 F2099. STOP RUN.                                                 PBONJO
000690 F2099-FN.                                                        PBONJO
000700     EXIT.                                                        PBONJO
000710 F20-FN.                                                          PBONJO
000720     EXIT.                                                        PBONJO
000730 F9099-ITER-FN.                                                   PBONJO
000740     GO TO F05.                                                   PBONJO
