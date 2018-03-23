000100921021     H DECEDIT('0,') DATEDIT(*DMY.)
000200970701     H* FNLV14R  *---------------------------------------------------*
000300970707     H*     CONTROLLO INDIRIZZO COMPLETO SENZA CAP                   *
000400970707     H*     PER MOTIVI DI PRATICITA' IL CAP VIENE CONTROLLATO A     *
000500970707     H*     PARTE - RICHIAMARE IL PGM FNLV13R
000600921021     H*--------------------------------------------------------------*
000700941019     FTABEL00F  IF   E           K DISK
000800020204     D ERR             S             78    DIM(11) CTDATA PERRCD(1)
000900960108     D CAR             S              1    DIM(10) CTDATA PERRCD(10)
001000960108     D K78             S              1    DIM(78)
001100061108
001200061108     d wiva            s                   like(e14pip) inz
001300061108     d wrag            s                   like(i14rsc) inz
001400061108     d wforzaiva       s              1    inz(*off)
001500940218      *
001600061107     **D* DS PER CONTROLLO DELLA PARTITA IVA - XIVAEUR
001700061107     **D XIVAEU        E DS
001800941019     D*
001900970701     D* DS PER TISI30R - INTERROGAZIONE LOCALITA'
002000970701     D DSSI00        E DS                  EXTNAME(TISI00DS)
002100020204     D*
002200020204     D* DS PER TIBS02R - CONTROLLO TABELLE
002300020204     D TIBS02        E DS                  EXTNAME(TIBS02DS)
002400941019     D*
002500941019     D* DS PER DATI IN ENTRATA
002600970701     D DSLV14        E DS                  EXTNAME(FNLV14DS)
002700061030     d* Per passaggio dati di output
002800061030     D ds15x         E DS                  EXTNAME(ds15)
002900061030     d                                     prefix(X_)
003000941019     D*
003100940919     D DS15          E DS
003200000121     D  W15LAD                56     58
003300020124     D  W15LAF                62     64
003400061107
003500061107     d xcfivads      e ds
003600061107     d  ivasta                 3      4
003700061107     d  ivapar                 5     18
003800941019     D*
003900941019     D KPJBA         E DS
004000941019     D                 DS
004100941221     D  WISO                   1      2
004200941221     D  WCPI                   3     16
004300941019     D  WPIVA                  1     16
004400020204     D PARAM1          DS
004500020204     D  P01NZD                 1      3
004600020214     D  P01PRV                 4      5
004700020214     D  P01ORD                 6      6
004800921021     C*---------------------------------------------------------------*
004900921021     C     *ENTRY        PLIST
005000970703     C                   PARM                    KPJBA
005100970703     C                   PARM                    DSLV14
005200061030     C                   PARM                    DS15x
005300000000     C*---------------------------------------------------------------*
005400921015     C     KTAB          KLIST
005500940218     C                   KFLD                    CODUT             1 0
005600921015     C                   KFLD                    COD               2
005700921015     C                   KFLD                    KEY               8
005800940919     C*
005900940919     C                   Z-ADD     1             CODUT
006000970701     C     *LIKE         DEFINE    E14NAR        WNAR
006100941019     C     *LIKE         DEFINE    TBLKEY        §KEY
006200970702     C     *LIKE         DEFINE    E14LOC        SAVLOC
006300940218     C*---------------------------------------------------------------*
006400970701     C* TIPO LANCIO, I14TLA "C" -           CHIUSO CON LR
006500970701     C* TIPO LANCIO, I14TLA "L" - ELABORO E CHIUDO CON LR
006600970701     C* TIPO LANCIO, I14TLA " " - ELABORO E CHIUDO IN RETRN
006700970701    1C     I14TLA        IFNE      'C'
006800940921     C*
006900970701     C                   CLEAR                   O14MSG
007000970701     C                   CLEAR                   O14ERR
007100061030     c                   if        %parms=3
007200061030     c                   clear                   ds15x
007300061030     c                   endif
007400061108      * se p.iva o ragione sociale diversi pulisco il flag di forzatura
007500061108     c                   if        wiva <> e14pip or wrag <> i14rsc
007600061108     c                   eval      wforzaiva = *off
007700061108     c                   endif
007800061108      * salvo ragione sociale
007900061108     c                   if        wrag = *blanks
008000061108     c                   eval      wrag = i14rsc
008100061108     c                   endif
008200941019     C*
008300941019     C* RAGIONE SOCIALE
008400970701     C     I14RSC        IFEQ      *BLANKS
008500970701     C     I14RS2        ANDEQ     *BLANKS
008600970701     C                   MOVEL     '1'           O14ERR
008700970701     C                   MOVEL     ERR(1)        O14MSG
008800941019     C                   GOTO      ENDC01
008900941019     C                   ENDIF
009000960108     C*
009100941025     C* SE IMMESSA SOLO LA 2 E NON LA 1 --> ERRORE
009200970701     C     I14RSC        IFEQ      *BLANKS
009300970701     C     I14RS2        ANDNE     *BLANKS
009400970701     C                   MOVEL     '1'           O14ERR
009500970701     C                   MOVEL     ERR(1)        O14MSG
009600941025     C                   GOTO      ENDC01
009700941025     C                   ENDIF
009800960108     C*
009900960108     C* CONTROLLO SE LA RAGIONE SOCIALE CONTIENE DEI CARATTERI NON
010000960108     C*  UTILIZZABILI
010100960108     C                   Z-ADD     1             X                 2 0
010200960108     C*
010300960108     C     X             DOWLE     10
010400960108     C     CAR(X)        ANDNE     *BLANKS
010500970701     C     CAR(X)        SCAN      I14RSC                                 31
010600970701     C  N31CAR(X)        SCAN      I14RS2                                 31
010700960108     C     *IN31         IFEQ      *ON
010800960108     C                   MOVEA     ERR(7)        K78
010900960108     C                   MOVEL     CAR(X)        K78(43)
011000970701     C                   MOVEA     K78           O14MSG
011100970701     C                   MOVEL     '1'           O14ERR
011200960108     C                   Z-ADD     10            X
011300960108     C                   ENDIF
011400960108     C                   ADD       1             X
011500960108     C                   ENDDO
011600960108     C*
011700960108     C   31              GOTO      ENDC01
011800941019     C* INDIRIZZO
011900970701     C     I14IND        IFEQ      *BLANKS
012000970701     C                   MOVEL     '2'           O14ERR
012100970701     C                   MOVEL     ERR(2)        O14MSG
012200941019     C                   GOTO      ENDC01
012300941019     C                   ENDIF
012400941019     C* LOCALITA
012500970707     C     E14LOC        IFEQ      *BLANKS
012600970707     C                   MOVEL     '3'           O14ERR
012700970707     C                   MOVEL     ERR(3)        O14MSG
012800970707     C                   GOTO      ENDC01
012900970707     C                   END
013000950116     C* NAZIONE
013100970701     C                   MOVEL     E14NAR        WNAR
013200941019     C                   EXSR      CTRNAR
013300970701     C                   MOVEL     WNAR          E14NAR
013400941019     C     WEMFOR        IFEQ      '1'
013500941019     C                   GOTO      ENDC01
013600941019     C                   ENDIF
013700970516     c*
013800970516     C* CONTROLLO SE IMMESSO FLAG INOLTRO
013900970702     C     I14ISO        IFNE      ' '
014000970516     C* CONTROLLO
014100970723     C                   MOVEL     '9A'          COD
014200970702     C                   MOVEL(P)  I14ISO        KEY
014300970516     C*
014400970516     C     KTAB          CHAIN     TABEL                              30
014500970516     C  N30TBLFLG        IFNE      ' '
014600970516     C                   SETON                                        30
014700970516     C                   END
014800970516     C*
014900970516    1C     *IN30         IFEQ      *ON
015000970701     C                   MOVEL     ERR(8)        O14MSG
015100970701     C                   MOVEL     '9'           O14ERR
015200970516     C                   GOTO      ENDC01
015300970516    1C                   ENDIF
015400970516     C                   ENDIF
015500941019     C*
015600941019     C* MEMORIZZO SE DESTINATARIO ITALIA O ESTERO
015700941019     C     §15ITA        IFNE      'I'
015800970701     C                   MOVEL     'E'           O14IE
015900941019     C                   ELSE
016000970701     C                   MOVEL     'I'           O14IE
016100941019     C                   ENDIF
016200941019     C* MEMORIZZO DIVISA DEL DESTINATARIO CHE MI PUO' SERVIRE X IL C/A
016300970701     C                   MOVEL     §15VLT        O14VLT
016400970701     C                   MOVEL     §15EFT        O14EFT
016500000321     C                   MOVEL     §15EFD        O14EFD
016600020124     C                   MOVEL     §15EFF        O14EFF
016700971215     C                   MOVEL     §15COD        O14COD
016800980219     C                   MOVEL     §15CPP        O14CPP
016900060921     c                   movel     §15red        o14red
017000000121     C     W15LAD        IFGT      *ZEROS
017100000121     C                   MOVEL     W15LAD        O14LAD
017200060919     c* memorizzo flag se vale solo per reso
017300000121     C                   ELSE
017400000121     C                   CLEAR                   O14LAD
017500000121     C                   ENDIF
017600020124     C*
017700020124     C     W15LAF        IFGT      *ZEROS
017800020124     C                   MOVEL     W15LAF        O14LAF
017900020124     C                   ELSE
018000020124     C                   CLEAR                   O14LAF
018100020124     C                   ENDIF
018200020130     C                   MOVEL     §15FFN        O14FFN
018300020206     C                   MOVEL     §15EEX        O14EEX
018400061030     c
018500061030     c* Se passata 3°parms, imposto la DS15X
018600061030     c                   if        %parms=3
018700061030     c                   movel     ds15          ds15x
018800061030     c                   endif
018900941129     C*
019000960327     C* CAP VUOTO  O CON '?' : INTERROGAZIONE SU CAPPARIO
019100980219     C* SOLO SE NAZIONE CON CAPPARIO
019200980219     C     O14CPP        IFEQ      *BLANKS
019300970701     C     '?'           SCAN      E14CAP                                 30
019400960327     C*
019500970701     C     E14CAP        IFEQ      *BLANKS
019600970701     C     E14PRV        ANDEQ     *BLANKS
019700960327     C     *IN30         OREQ      *ON
019800970701     C                   CLEAR                   DSSI00
019900970703     C                   MOVEL     'L01'         D00OP0
020000970701     C                   MOVEL     E14LOC        D00LOC
020100970701     C                   MOVEL     I14DRI        D00DRI
020200980220     C                   MOVEL     E14NAR        D00NAR
020300970701     C                   CALL      'TISI30R'
020400941129     C                   PARM                    KPJBA
020500970701     C                   PARM                    DSSI00
020600970701     C* SE SELEZIONATO CAP
020700970701     C     D00CAP        IFNE      *BLANKS
020800970701     C                   MOVEL     D00CAP        E14CAP
020900970701     C                   MOVEL     D00LOC        E14LOC
021000970701     C                   MOVEL     D00NAR        E14NAR
021100970704     C                   MOVEL     D00PRV        E14PRV
021200941129     C                   END
021300970704     C**
021400970701     C                   MOVEL     '5'           O14ERR
021500941129     C                   GOTO      ENDC01
021600941129     C*
021700941129     C                   END
021800020204     C                   ELSE
021900020204     C* NAZIONE SENZA CAPPARIO
022000020204     C* GESTISCO OBBLIGATORIETA' DELLO STATO
022100020205     C     I14CNZ        IFEQ      'S'
022200020205    2C     §15STO        ANDEQ     'S'
022300020204     C* SE NON IMMESSO RICHIAMO PGM DI INTERROGAZIONE/SCELTA
022400020204    3C     E14PRV        IFEQ      *BLANKS
022500020204     C                   MOVE      KPJBU         WA256           256
022600020204     C                   CLEAR                   PARAM1
022700020204     C                   MOVEL     E14NAR        P01NZD
022800020204     C                   MOVEL     '1'           P01ORD
022900020204     C                   MOVEL     PARAM1        KPJBU
023000020204     C                   CALL      'TNTB20R'
023100020204     C                   PARM                    KPJBA
023200020204     C                   MOVEL     KPJBU         PARAM1
023300020204     C                   MOVEL     P01PRV        E14PRV
023400020204     C                   MOVE      WA256         KPJBU
023500020204     C                   MOVEL     '0'           O14ERR
023600020204     C                   GOTO      ENDC01
023700020204   X3C                   ELSE
023800020204     C* ALTRIMENTI DEVE ESISTERE SU TABELLA STA
023900020204     C                   CLEAR                   TIBS02
024000020204     C                   MOVEL     'C'           T02MOD
024100020204     C                   MOVEL     'STA'         T02COD
024200020204     C                   MOVEL(P)  E14NAR        T02KE1
024300020204     C                   MOVEL(P)  E14PRV        T02KE2
024400020204     C                   MOVEL     KNSIF         T02SIF
024500020204     C                   CALL      'TIBS02R'
024600020204     C                   PARM                    KPJBA
024700020204     C                   PARM                    TIBS02
024800020204     C     T02ERR        IFEQ      'E'
024900020204     C     T02ERR        OREQ      'A'
025000020204     C                   MOVEL     '0'           O14ERR
025100020204     C                   MOVEL     ERR(11)       O14MSG
025200020204     C                   GOTO      ENDC01
025300020204     C                   ENDIF
025400020204    3C                   ENDIF
025500020204    2C                   ENDIF
025600980219     C                   END
025700941020     C*
025800950116     C* PARTITA IVA
025900970701     C                   MOVEL     E14PIP        WPIVA
026000950116     C                   EXSR      CTRPIV
026100970701     C                   MOVEL     WPIVA         E14PIP
026200950116     C     WEMFOR        IFEQ      '1'
026300950116     C                   GOTO      ENDC01
026400950116     C                   ENDIF
026500941019     C                   ENDIF
026600941019     C*
026700941019     C     ENDC01        TAG
026800941019     C* CHIUSURA PGM
026900970701    1C     I14TLA        IFEQ      ' '
027000941019     C                   RETURN
027100941019     C                   ELSE
027200941019     C                   SETON                                        LR
027300941019    1C                   ENDIF
027400940919     C*
027500950116     C* CONTROLLO NAZIONE --------------------------------------------*
027600941019     C     CTRNAR        BEGSR
027700941129     C*
027800941129     C                   CLEAR                   WEMFOR
027900941019     C*
028000941019     C                   MOVEL     '15'          COD
028100941019     C* RICERCA
028200941019     C     '?'           SCAN      WNAR                                   30
028300941019    1C     *IN30         IFEQ      *ON
028400020130     C                   CALL      'TRUL19R'
028500020130     C                   PARM      '15'          CODTAB            2
028600020305     C                   PARM      '1'           ORDINA            1
028700020130     C                   PARM                    KEYTAB            8
028800020130     C                   PARM                    CMD               1
028900941019     C                   MOVEL     '1'           WEMFOR            1
029000020130     C                   MOVEL     KEYTAB        WNAR
029100970701     C                   MOVEL     '6'           O14ERR
029200950116     C                   GOTO      ENDNAZ
029300941019    1C                   ENDIF
029400941019     C* CONTROLLO
029500941019     C                   MOVEL(P)  WNAR          KEY
029600941019     C*
029700941019     C     KTAB          CHAIN     TABEL                              30
029800941019     C  N30TBLFLG        IFNE      ' '
029900941019     C                   SETON                                        30
030000941019     C                   END
030100941019     C*
030200941019    1C     *IN30         IFEQ      *ON
030300970701     C                   MOVEL     ERR(6)        O14MSG
030400970701     C                   MOVEL     '6'           O14ERR
030500950116     C                   GOTO      ENDNAZ
030600941019    1C                   ENDIF
030700941019     C*
030800941019     C                   MOVEL     TBLUNI        DS15
030900941123     C*
031000941123     C     §15ITA        IFEQ      'I'
031100941123     C                   CLEAR                   WNAR
031200941123     C                   ENDIF
031300950116     C     ENDNAZ        TAG
031400950116     C*
031500970701     C     O14ERR        IFNE      *BLANKS
031600950116     C                   MOVEL     '1'           WEMFOR
031700950116    1C                   END
031800950116     C                   ENDSR
031900941019     C*
032000950116     C* CONTROLLO P.IVA ----------------------------------------------*
032100950116     C     CTRPIV        BEGSR
032200950116     C*
032300950116     C                   CLEAR                   WEMFOR
032400950116     C*
032500941019     C* PARTITA IVA
032600941019    1C     WPIVA         IFNE      *BLANKS
032700941019     C* PROGRAMMA CHE CONTROLLA LA P.IVA
032800061107     c                   clear                   xcfivads
032900061107     c                   eval      xcfivamod = 'P'
033000061107     c                   eval      xcfivapar = wpiva
033100061107     c                   eval      xcfivanar = wnar
033200061107     c                   eval      xcfivaprv = e14prv
033300061107     c                   call      'XCFIVAR'
033400061107     c                   parm                    xcfivads
033500100115     c                   parm                    i14forzait
033600100115     c
033700061107     **C                   CLEAR                   XIVAEU
033800061107     **c                   Clear                   xivaprv           2
033900061107     **C*
034000061107     **C                   MOVEL     WNAR          IVANAR
034100061107     **C                   MOVEL     WCPI          IVAPAR
034200061107     **C                   MOVEL     WISO          IVASTA
034300061107     **c                   Movel     e14prv        xivaprv
034400061107     **C*
034500061107     **C                   CALL      'XIVAEUR'
034600061107     **C                   PARM                    XIVAEU
034700061107     **c                   Parm                    xivaprv
034800950613     C* SE SI TRATTA DI UN PRIVATO NON C'E' LA OBBLIGATORIETA
034900950613     C*  NEMMENO PER L'ESTERO
035000950613    2C     WISO          IFNE      '$$'
035100950116     C*
035200950116     C* SE NON IMMESSO CODICE ISO E IMPOSTATO AUTOMATICAMENTE DALLA
035300950116     C*  NAZIONE --> LO EVIDENZIO
035400950613    3C     IVASTA        IFNE      WISO
035500950116     C                   MOVEL     IVASTA        WISO
035600970701     C                   MOVEL     '8'           O14ERR
035700950613    3C                   ENDIF
035800950616    2C                   ENDIF
035900941019     C*
036000061107    3**C     IVAFLG        IFLT      0
036100061108      * frozabile
036200061108     c                   if        xcfivaflg = -9 and wforzaiva = *off and
036300061108     c                             wiva <> e14pip
036400061108     c                   eval      o14err = '7'
036500061108     c                   eval      o14msg = xcfivamsg
036600061108     c                   eval      o14msg = %trim(o14msg) + ' Enter x forzare'
036700061108     c                   eval      wforzaiva = *on
036800061108     c                   eval      wiva = e14pip
036900061108     c                   goto      endcna
037000061108     c                   endif
037100061108      * bloccante
037200061108     c                   if        xcfivaflg < 0 and xcfivaflg <> -9
037300061107     C                   MOVEL     xcfIVAMSG     O14MSG
037400970701     C                   MOVEL     '7'           O14ERR
037500941019     C                   GOTO      ENDCNA
037600950613    3C                   ENDIF
037700941019     C*
037800950613     C*
037900950613   X1C**                   ELSE
038000950613     C* SE ESTERO OBBLIGATORIO SE C'E' ATTRAVERSAMENTO DOGANA
038100970701    2C**         O14IE     IFEQ 'E'
038200970701     C**         O14EFT    ANDNE*BLANKS
038300970701     C**                   MOVELERR,5     O14MSG
038400970701     C**                   MOVEL'7'       O14ERR
038500950613    2C**                   ENDIF
038600950613    1C                   ENDIF
038700941019     C*
038800941019     C     ENDCNA        TAG
038900941019     C*
039000970701     C     O14ERR        IFNE      *BLANKS
039100941019     C                   MOVEL     '1'           WEMFOR
039200941019    1C                   END
039300941019     C                   ENDSR
039400020213**
039500941019Manca Ragione Sociale
039600941019Manca Indirizzo
039700941019Manca localita'
039800941019Manca provincia
039900950613Partita IVA obbligatoria se bolla diretta all'estero con attraversam.dogana
040000941019Nazione Errata
040100960108La ragione sociale contiene il carattere  x  non valido!!
040200970516Inoltro errato
040300970703C.A.P. incongruente con provincia e localita'                                  9
040400970702C.A.P. inesistente                                                            10
040500020204Codice Stato inesistente o annullato
040600960108**
040700020213§!:@
