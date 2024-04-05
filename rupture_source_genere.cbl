000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.  PAPRUP.                                             PAPRUP
000030 AUTHOR.         premier programme rupture.                       PAPRUP
000040 DATE-COMPILED.   28/02/00.                                       PAPRUP
000050 ENVIRONMENT DIVISION.                                            PAPRUP
000060 CONFIGURATION SECTION.                                           PAPRUP
000070 SOURCE-COMPUTER. IBM-370.                                        PAPRUP
000080 OBJECT-COMPUTER. IBM-370.                                        PAPRUP
000090 INPUT-OUTPUT SECTION.                                            PAPRUP
000100 FILE-CONTROL.                                                    PAPRUP
000110      SELECT     TC-FICHIER    ASSIGN    UT-S-INP001.             PAPRUP
000120 DATA DIVISION.                                                   PAPRUP
000130 FILE SECTION.                                                    PAPRUP
000140 FD                 TC-FICHIER                                    PAPRUP
000150      BLOCK              00000 RECORDS                            PAPRUP
000160      DATA RECORD                                                 PAPRUP
000170                    TC00                                          PAPRUP
000180           LABEL RECORD STANDARD.                                 PAPRUP
000190 01               TC00.                                           PAPRUP
000200   10             TC00-NOCPTE   PICTURE 9(10).                    PAPRUP
000210   10             TC00-DAMVT    PICTURE X(8).                     PAPRUP
000220   10             TC00-CDMVT    PICTURE X.                        PAPRUP
000230   10             TC00-MTMVT    PICTURE S9(10).                   PAPRUP
000240   10             TC00-FILLER   PICTURE X(21).                    PAPRUP
000250 WORKING-STORAGE SECTION.                                         PAPRUP
000260 01               WA0I-MTMVT    PICTURE S9(10).                   7WA100
000270 01               WA0S-MTMVT    PICTURE --B---B---B--9.           7WA110
000280 01                              DEBUT-WSS.                       PAPRUP
000290   05             FILLER        PICTURE X(7) VALUE                PAPRUP
000300                                'WORKING'.                        PAPRUP
000310   05             IK            PICTURE X.                        PAPRUP
000320 01               CONSTANTES-PAC.                                 PAPRUP
000330   05             FILLER        PICTURE X(60) VALUE               PAPRUP
000340     '0096 LBA28/02/00PAPRUPTEST    12:03:09PAPRUP  BVAP28/02/2000PAPRUP
000350-    ''.                                                           PAPRUP
000360 01               PAC-CONSTANTES REDEFINES CONSTANTES-PAC.        PAPRUP
000370   05             NUGNA         PICTURE X(5).                     PAPRUP
000380   05             APPLI         PICTURE X(3).                     PAPRUP
000390   05             DATGN         PICTURE X(8).                     PAPRUP
000400   05             PROGR         PICTURE X(6).                     PAPRUP
000410   05             CODUTI        PICTURE X(8).                     PAPRUP
000420   05             TIMGN         PICTURE X(8).                     PAPRUP
000430   05             PROGE         PICTURE X(8).                     PAPRUP
000440   05             COBASE        PICTURE X(4).                     PAPRUP
000450   05             DATGNC        PICTURE X(10).                    PAPRUP
000460 01               DATCE.                                          PAPRUP
000470   05             CENTUR        PICTURE XX VALUE                  PAPRUP
000480                                '19'.                             PAPRUP
000490   05             DATOR.                                          PAPRUP
000500     10           DATOA         PICTURE XX.                       PAPRUP
000510     10           DATOM         PICTURE XX.                       PAPRUP
000520     10           DATOJ         PICTURE XX.                       PAPRUP
000530 01               VARIABLES-CONDITIONNELLES.                      PAPRUP
000540   05             RTD.                                            PAPRUP
000550     10           RTD1          PICTURE X VALUE                   PAPRUP
000560                                '1'.                              PAPRUP
000570   05             NRD           PICTURE 9 VALUE 1.                PAPRUP
000580   05             NRP           PICTURE 9 VALUE ZERO.             PAPRUP
000590   05             RTP.                                            PAPRUP
000600     10           RTP1          PICTURE X VALUE                   PAPRUP
000610                                '1'.                              PAPRUP
000620   05             TC-DE.                                          PAPRUP
000630     10           TC-DE1        PICTURE X VALUE                   PAPRUP
000640                                '1'.                              PAPRUP
000650   05             TC-PE.                                          PAPRUP
000660     10           TC-PE1        PICTURE X VALUE                   PAPRUP
000670                                '1'.                              PAPRUP
000680   05             FT.                                             PAPRUP
000690     10           TC-FT         PICTURE X VALUE                   PAPRUP
000700                                '0'.                              PAPRUP
000710   05             FI.                                             PAPRUP
000720     10           TC-FI         PICTURE X VALUE                   PAPRUP
000730                                '0'.                              PAPRUP
000740 01               COMPTEURS-FICHIERS COMPUTATIONAL-3.             PAPRUP
000750   05             5-TC00-CPTENR PICTURE S9(9) VALUE ZERO.         PAPRUP
000760 01               1-TC00.                                         PAPRUP
000770   10             1-TC00-NOCPTE PICTURE 9(10).                    PAPRUP
000780   10             1-TC00-DAMVT  PICTURE X(8).                     PAPRUP
000790   10             1-TC00-CDMVT  PICTURE X.                        PAPRUP
000800   10             1-TC00-MTMVT  PICTURE S9(10).                   PAPRUP
000810   10             1-TC00-FILLER PICTURE X(21).                    PAPRUP
000820 01               ZONES-UTILISATEUR PICTURE X.                    PAPRUP
000830 PROCEDURE DIVISION.                                              PAPRUP
000840 N01.                                                             PAPRUP
000850           NOTE *************************************.            PAPRUP
000860                *                                   *             PAPRUP
000870                *INITIALISATIONS                    *             PAPRUP
000880                *                                   *             PAPRUP
000890                *************************************.            PAPRUP
000900 F01.                                                             PAPRUP
000910     EXIT.                                                        PAPRUP
000920 N01TC.                                                           PAPRUP
000930           NOTE *INITIALISATION FICHIER  TC-FICHIER *.            PAPRUP
000940 F01TC.                                                           PAPRUP
000950     OPEN INPUT TC-FICHIER.                                       PAPRUP
000960 F01TC-10.                                                        PAPRUP
000970     READ TC-FICHIER AT END                                       PAPRUP
000980         MOVE 1 TO TC-FI.                                         PAPRUP
000990 F01TC-FN.                                                        PAPRUP
001000     EXIT.                                                        PAPRUP
001010 F01-FN.                                                          PAPRUP
001020     EXIT.                                                        PAPRUP
001030*          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            PAPRUP
001040 F05.                                                             PAPRUP
001050     EXIT.                                                        PAPRUP
001060 N10.                                                             PAPRUP
001070           NOTE *************************************.            PAPRUP
001080                *                                   *             PAPRUP
001090                *LECTURE FICHIERS ACCES SEQ. AVEC DE*             PAPRUP
001100                *                                   *             PAPRUP
001110                *************************************.            PAPRUP
001120 F10.                                                             PAPRUP
001130     EXIT.                                                        PAPRUP
001140 N10TC.                                                           PAPRUP
001150           NOTE *LECTURE FICHIER         TC  AVEC DE*.            PAPRUP
001160 F10TC-10.                                                        PAPRUP
001170     MOVE TC-DE TO TC-PE.                                         PAPRUP
001180     MOVE NRD TO NRP.                                             PAPRUP
001190     IF      TC-FI  =  '1'                                        PAPRUP
001200         MOVE 1 TO TC-FT                                          PAPRUP
001210         GO TO F10TC-FN.                                          PAPRUP
001220     MOVE TC00 TO 1-TC00.                                         PAPRUP
001230     ADD 1 TO 5-TC00-CPTENR.                                      PAPRUP
001240     READ TC-FICHIER AT END                                       PAPRUP
001250         MOVE 1 TO TC-FI.                                         PAPRUP
001260 F10TC-FN.                                                        PAPRUP
001270     EXIT.                                                        PAPRUP
001280 F10-FN.                                                          PAPRUP
001290     EXIT.                                                        PAPRUP
001300 N20.                                                             PAPRUP
001310           NOTE *************************************.            PAPRUP
001320                *                                   *             PAPRUP
001330                *FIN DE TRAITEMENT                  *             PAPRUP
001340                *                                   *             PAPRUP
001350                *************************************.            PAPRUP
001360 F20.                                                             PAPRUP
001370     IF      FT  =  ALL '1'                                       PAPRUP
001380         NEXT SENTENCE                                            PAPRUP
001390     ELSE                                                         PAPRUP
001400         GO TO F20-FN.                                            PAPRUP
001410 F20TC.                                                           PAPRUP
001420     CLOSE TC-FICHIER.                                            PAPRUP
001430 F20TC-FN.                                                        PAPRUP
001440     EXIT.                                                        PAPRUP
001450 F2099. STOP RUN.                                                 PAPRUP
001460 F2099-FN.                                                        PAPRUP
001470     EXIT.                                                        PAPRUP
001480 F20-FN.                                                          PAPRUP
001490     EXIT.                                                        PAPRUP
001500 N22.                                                             PAPRUP
001510           NOTE *************************************.            PAPRUP
001520                *                                   *             PAPRUP
001530                *CALCUL DES DERNIERS ENREGISTREMENTS*             PAPRUP
001540                *                                   *             PAPRUP
001550                *************************************.            PAPRUP
001560 F22.                                                             PAPRUP
001570     EXIT.                                                        PAPRUP
001580 N22TC.                                                           PAPRUP
001590           NOTE *CALCUL DE SUR FICHIER   TC-FICHIER *.            PAPRUP
001600 F22TC.                                                           PAPRUP
001610     MOVE ZERO TO TC-DE.                                          PAPRUP
001620     MOVE ZERO TO NRD.                                            PAPRUP
001630     MOVE RTD TO RTP.                                             PAPRUP
001640     MOVE ZERO TO RTD.                                            PAPRUP
001650     IF      TC-FI  =  '1'  MOVE 1 TO NRD                         PAPRUP
001660         GO TO F22TC-1.                                           PAPRUP
001670     IF      TC00-NOCPTE NOT  =  1-TC00-NOCPTE                    PAPRUP
001680         MOVE 1 TO NRD                                            PAPRUP
001690         GO TO F22TC-1.                                           PAPRUP
001700     GO TO F22TC-FN.                                              PAPRUP
001710 F22TC-1.                                                         PAPRUP
001720     MOVE 1 TO TC-DE1.                                            PAPRUP
001730     MOVE TC-DE TO RTD.                                           PAPRUP
001740 F22TC-FN.                                                        PAPRUP
001750     EXIT.                                                        PAPRUP
001760 F22-FN.                                                          PAPRUP
001770     EXIT.                                                        PAPRUP
001780 N70.                                                             P000
001790           NOTE *************************************.            P000
001800                *                                   *             P000
001810                *CONTROLE                           *             P000
001820                *                                   *             P000
001830                *************************************.            P000
001840 F70.                                                             P000
001850     EXIT.                                                        P000
001860 N70CA.                                                           P000
001870           NOTE *INIT                               *.            P000
001880 F70CA.                                                           P000
001890     IF      RTP1  =  '1'                                         P000
001900         NEXT SENTENCE                                            P000
001910     ELSE                                                         P000
001920         GO TO F70CA-FN.                                          P000
001930     MOVE ZERO TO WA0I-MTMVT.                                     P100
001940 F70CA-FN.                                                        P100
001950     EXIT.                                                        P100
001960 N70DA.                                                           P000
001970           NOTE *CONTROLE                           *.            P000
001980 F70DA.                                                           P000
001990     IF      1-TC00-CDMVT NOT  =  'R'                             P000
002000         AND 1-TC00-CDMVT NOT  =  'C'                             P020
002010         AND 1-TC00-CDMVT NOT  =  'D'                             P040
002020             NEXT SENTENCE                                        P040
002030     ELSE                                                         P040
002040             GO TO F70DA-FN.                                      P040
002050     DISPLAY 'ERREUR SUR LE SENS DU MVT'                          P100
002060     1-TC00-CDMVT ' '                                             P110
002070     'POUR LE COMPTE '                                            P120
002080     DISPLAY 1-TC00-NOCPTE                                        P130
002090     GO TO F05.                                                   P900
002100 F70DA-FN.                                                        P900
002110     EXIT.                                                        P900
002120 N70EA.                                                           P000
002130           NOTE *CALCUL                             *.            P000
002140 F70EA.                                                           P000
002150     IF      1-TC00-CDMVT  =  'D'                                 P100
002160         ADD 1-TC00-MTMVT TO WA0I-MTMVT                           P100
002170     ELSE                                                         P200
002180         SUBTRACT 1-TC00-MTMVT FROM WA0I-MTMVT.                   P200
002190 F70EA-FN.                                                        P200
002200     EXIT.                                                        P200
002210 N70FA.                                                           P000
002220           NOTE *FIN DU COMPTE                      *.            P000
002230 F70FA.                                                           P000
002240     IF      RTD1  =  '1'                                         P000
002250         NEXT SENTENCE                                            P000
002260     ELSE                                                         P000
002270         GO TO F70FA-FN.                                          P000
002280     MOVE WA0I-MTMVT TO WA0S-MTMVT                                P050
002290     DISPLAY 'TOTAL DES MOUVEMENTS'                               P100
002300     WA0S-MTMVT ' '                                               P120
002310     'POUR LE COMPTE  '                                           P220
002320     1-TC00-NOCPTE.                                               P230
002330 F70FA-FN.                                                        P230
002340     EXIT.                                                        P230
002350 F70-FN.                                                          P230
002360     EXIT.                                                        P230
002370 F9099-ITER-FN.                                                   PAPRUP
002380     GO TO F05.                                                   PAPRUP
