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
003000110713     D ds9a          E DS
003100941019     D*
003200940919     D DS15          E DS
003300000121     D  W15LAD                56     58
003400020124     D  W15LAF                62     64
003500061107
003600061107     d xcfivads      e ds
003700061107     d  ivasta                 3      4
003800061107     d  ivapar                 5     18
003900941019     D*
004000941019     D KPJBA         E DS
004100941019     D                 DS
004200941221     D  WISO                   1      2
004300941221     D  WCPI                   3     16
004400941019     D  WPIVA                  1     16
004500020204     D PARAM1          DS
004600020204     D  P01NZD                 1      3
004700020214     D  P01PRV                 4      5
004800020214     D  P01ORD                 6      6
004900921021     C*---------------------------------------------------------------*
005000921021     C     *ENTRY        PLIST
005100970703     C                   PARM                    KPJBA
005200970703     C                   PARM                    DSLV14
005300061030     C                   PARM                    DS15x
005400000000     C*---------------------------------------------------------------*
005500921015     C     KTAB          KLIST
005600940218     C                   KFLD                    CODUT             1 0
005700921015     C                   KFLD                    COD               2
005800921015     C                   KFLD                    KEY               8
005900940919     C*
006000940919     C                   Z-ADD     1             CODUT
006100970701     C     *LIKE         DEFINE    E14NAR        WNAR
006200941019     C     *LIKE         DEFINE    TBLKEY        §KEY
006300970702     C     *LIKE         DEFINE    E14LOC        SAVLOC
006400940218     C*---------------------------------------------------------------*
006500970701     C* TIPO LANCIO, I14TLA "C" -           CHIUSO CON LR
006600970701     C* TIPO LANCIO, I14TLA "L" - ELABORO E CHIUDO CON LR
006700970701     C* TIPO LANCIO, I14TLA " " - ELABORO E CHIUDO IN RETRN
006800970701    1C     I14TLA        IFNE      'C'
006900940921     C*
007000970701     C                   CLEAR                   O14MSG
007100970701     C                   CLEAR                   O14ERR
007200061030     c                   if        %parms=3
007300061030     c                   clear                   ds15x
007400061030     c                   endif
007500061108      * se p.iva o ragione sociale diversi pulisco il flag di forzatura
007600061108     c                   if        wiva <> e14pip or wrag <> i14rsc
007700061108     c                   eval      wforzaiva = *off
007800061108     c                   endif
007900061108      * salvo ragione sociale
008000061108     c                   if        wrag = *blanks
008100061108     c                   eval      wrag = i14rsc
008200061108     c                   endif
008300941019     C*
008400941019     C* RAGIONE SOCIALE
008500970701     C     I14RSC        IFEQ      *BLANKS
008600970701     C     I14RS2        ANDEQ     *BLANKS
008700970701     C                   MOVEL     '1'           O14ERR
008800970701     C                   MOVEL     ERR(1)        O14MSG
008900941019     C                   GOTO      ENDC01
009000941019     C                   ENDIF
009100960108     C*
009200941025     C* SE IMMESSA SOLO LA 2 E NON LA 1 --> ERRORE
009300970701     C     I14RSC        IFEQ      *BLANKS
009400970701     C     I14RS2        ANDNE     *BLANKS
009500970701     C                   MOVEL     '1'           O14ERR
009600970701     C                   MOVEL     ERR(1)        O14MSG
009700941025     C                   GOTO      ENDC01
009800941025     C                   ENDIF
009900960108     C*
010000960108     C* CONTROLLO SE LA RAGIONE SOCIALE CONTIENE DEI CARATTERI NON
010100960108     C*  UTILIZZABILI
010200960108     C                   Z-ADD     1             X                 2 0
010300960108     C*
010400110920     C*    X             DOWLE     10
010500110920     C*    CAR(X)        ANDNE     *BLANKS
010600110920     C*    CAR(X)        SCAN      I14RSC                                 31
010700110920     C* N31CAR(X)        SCAN      I14RS2                                 31
010800110920     C*    *IN31         IFEQ      *ON
010900110920     C*                  MOVEA     ERR(7)        K78
011000110920     C*                  MOVEL     CAR(X)        K78(43)
011100110920     C*                  MOVEA     K78           O14MSG
011200110920     C*                  MOVEL     '1'           O14ERR
011300110920     C*                  Z-ADD     10            X
011400110920     C*                  ENDIF
011500110920     C*                  ADD       1             X
011600110920     C*                  ENDDO
011700960108     C*
011800110920     C*  31              GOTO      ENDC01
011900941019     C* INDIRIZZO
012000970701     C     I14IND        IFEQ      *BLANKS
012100970701     C                   MOVEL     '2'           O14ERR
012200970701     C                   MOVEL     ERR(2)        O14MSG
012300941019     C                   GOTO      ENDC01
012400941019     C                   ENDIF
012500941019     C* LOCALITA
012600970707     C     E14LOC        IFEQ      *BLANKS
012700970707     C                   MOVEL     '3'           O14ERR
012800970707     C                   MOVEL     ERR(3)        O14MSG
012900970707     C                   GOTO      ENDC01
013000970707     C                   END
013100950116     C* NAZIONE
013200970701     C                   MOVEL     E14NAR        WNAR
013300941019     C                   EXSR      CTRNAR
013400970701     C                   MOVEL     WNAR          E14NAR
013500941019     C     WEMFOR        IFEQ      '1'
013600941019     C                   GOTO      ENDC01
013700941019     C                   ENDIF
013800970516     c*
013900970516     C* CONTROLLO SE IMMESSO FLAG INOLTRO
014000970702     C     I14ISO        IFNE      ' '
014100970516     C* CONTROLLO
014200970723     C                   MOVEL     '9A'          COD
014300970702     C                   MOVEL(P)  I14ISO        KEY
014400970516     C*
014500970516     C     KTAB          CHAIN     TABEL                              30
014600970516     C  N30TBLFLG        IFNE      ' '
014700970516     C                   SETON                                        30
014800970516     C                   END
014900970516     C*
015000970516    1C     *IN30         IFEQ      *ON
015100970701     C                   MOVEL     ERR(8)        O14MSG
015200970701     C                   MOVEL     '9'           O14ERR
015300970516     C                   GOTO      ENDC01
015400970516    1C                   ENDIF
015500110713     c
015600110713     c                   movel     tbluni        ds9a
015700110713     c* Inoltro non utilizzabile nei pgm operativi
015800110713     c                   if        §9autiop='N'
015900110713     C                   MOVEL     ERR(8)        O14MSG
016000110713     C                   MOVEL     '9'           O14ERR
016100110713     C                   GOTO      ENDC01
016200110713    1C                   ENDIF
016300970516     C                   ENDIF
016400941019     C*
016500941019     C* MEMORIZZO SE DESTINATARIO ITALIA O ESTERO
016600941019     C     §15ITA        IFNE      'I'
016700970701     C                   MOVEL     'E'           O14IE
016800941019     C                   ELSE
016900970701     C                   MOVEL     'I'           O14IE
017000941019     C                   ENDIF
017100941019     C* MEMORIZZO DIVISA DEL DESTINATARIO CHE MI PUO' SERVIRE X IL C/A
017200970701     C                   MOVEL     §15VLT        O14VLT
017300970701     C                   MOVEL     §15EFT        O14EFT
017400000321     C                   MOVEL     §15EFD        O14EFD
017500020124     C                   MOVEL     §15EFF        O14EFF
017600971215     C                   MOVEL     §15COD        O14COD
017700980219     C                   MOVEL     §15CPP        O14CPP
017800060921     c                   movel     §15red        o14red
017900000121     C     W15LAD        IFGT      *ZEROS
018000000121     C                   MOVEL     W15LAD        O14LAD
018100060919     c* memorizzo flag se vale solo per reso
018200000121     C                   ELSE
018300000121     C                   CLEAR                   O14LAD
018400000121     C                   ENDIF
018500020124     C*
018600020124     C     W15LAF        IFGT      *ZEROS
018700020124     C                   MOVEL     W15LAF        O14LAF
018800020124     C                   ELSE
018900020124     C                   CLEAR                   O14LAF
019000020124     C                   ENDIF
019100020130     C                   MOVEL     §15FFN        O14FFN
019200020206     C                   MOVEL     §15EEX        O14EEX
019300061030     c
019400061030     c* Se passata 3°parms, imposto la DS15X
019500061030     c                   if        %parms=3
019600061030     c                   movel     ds15          ds15x
019700061030     c                   endif
019800941129     C*
019900960327     C* CAP VUOTO  O CON '?' : INTERROGAZIONE SU CAPPARIO
020000980219     C* SOLO SE NAZIONE CON CAPPARIO
020100980219     C     O14CPP        IFEQ      *BLANKS
020200970701     C     '?'           SCAN      E14CAP                                 30
020300960327     C*
020400970701     C     E14CAP        IFEQ      *BLANKS
020500970701     C     E14PRV        ANDEQ     *BLANKS
020600960327     C     *IN30         OREQ      *ON
020700970701     C                   CLEAR                   DSSI00
020800970703     C                   MOVEL     'L01'         D00OP0
020900970701     C                   MOVEL     E14LOC        D00LOC
021000970701     C                   MOVEL     I14DRI        D00DRI
021100980220     C                   MOVEL     E14NAR        D00NAR
021200970701     C                   CALL      'TISI30R'
021300941129     C                   PARM                    KPJBA
021400970701     C                   PARM                    DSSI00
021500970701     C* SE SELEZIONATO CAP
021600970701     C     D00CAP        IFNE      *BLANKS
021700970701     C                   MOVEL     D00CAP        E14CAP
021800970701     C                   MOVEL     D00LOC        E14LOC
021900970701     C                   MOVEL     D00NAR        E14NAR
022000970704     C                   MOVEL     D00PRV        E14PRV
022100941129     C                   END
022200970704     C**
022300970701     C                   MOVEL     '5'           O14ERR
022400941129     C                   GOTO      ENDC01
022500941129     C*
022600941129     C                   END
022700020204     C                   ELSE
022800020204     C* NAZIONE SENZA CAPPARIO
022900020204     C* GESTISCO OBBLIGATORIETA' DELLO STATO
023000020205     C     I14CNZ        IFEQ      'S'
023100020205    2C     §15STO        ANDEQ     'S'
023200020204     C* SE NON IMMESSO RICHIAMO PGM DI INTERROGAZIONE/SCELTA
023300020204    3C     E14PRV        IFEQ      *BLANKS
023400020204     C                   MOVE      KPJBU         WA256           256
023500020204     C                   CLEAR                   PARAM1
023600020204     C                   MOVEL     E14NAR        P01NZD
023700020204     C                   MOVEL     '1'           P01ORD
023800020204     C                   MOVEL     PARAM1        KPJBU
023900020204     C                   CALL      'TNTB20R'
024000020204     C                   PARM                    KPJBA
024100020204     C                   MOVEL     KPJBU         PARAM1
024200020204     C                   MOVEL     P01PRV        E14PRV
024300020204     C                   MOVE      WA256         KPJBU
024400020204     C                   MOVEL     '0'           O14ERR
024500020204     C                   GOTO      ENDC01
024600020204   X3C                   ELSE
024700020204     C* ALTRIMENTI DEVE ESISTERE SU TABELLA STA
024800020204     C                   CLEAR                   TIBS02
024900020204     C                   MOVEL     'C'           T02MOD
025000020204     C                   MOVEL     'STA'         T02COD
025100020204     C                   MOVEL(P)  E14NAR        T02KE1
025200020204     C                   MOVEL(P)  E14PRV        T02KE2
025300020204     C                   MOVEL     KNSIF         T02SIF
025400020204     C                   CALL      'TIBS02R'
025500020204     C                   PARM                    KPJBA
025600020204     C                   PARM                    TIBS02
025700020204     C     T02ERR        IFEQ      'E'
025800020204     C     T02ERR        OREQ      'A'
025900020204     C                   MOVEL     '0'           O14ERR
026000020204     C                   MOVEL     ERR(11)       O14MSG
026100020204     C                   GOTO      ENDC01
026200020204     C                   ENDIF
026300020204    3C                   ENDIF
026400020204    2C                   ENDIF
026500980219     C                   END
026600941020     C*
026700950116     C* PARTITA IVA
026800970701     C                   MOVEL     E14PIP        WPIVA
026900950116     C                   EXSR      CTRPIV
027000970701     C                   MOVEL     WPIVA         E14PIP
027100950116     C     WEMFOR        IFEQ      '1'
027200950116     C                   GOTO      ENDC01
027300950116     C                   ENDIF
027400941019     C                   ENDIF
027500941019     C*
027600941019     C     ENDC01        TAG
027700941019     C* CHIUSURA PGM
027800970701    1C     I14TLA        IFEQ      ' '
027900941019     C                   RETURN
028000941019     C                   ELSE
028100941019     C                   SETON                                        LR
028200941019    1C                   ENDIF
028300940919     C*
028400950116     C* CONTROLLO NAZIONE --------------------------------------------*
028500941019     C     CTRNAR        BEGSR
028600941129     C*
028700941129     C                   CLEAR                   WEMFOR
028800941019     C*
028900941019     C                   MOVEL     '15'          COD
029000941019     C* RICERCA
029100941019     C     '?'           SCAN      WNAR                                   30
029200941019    1C     *IN30         IFEQ      *ON
029300020130     C                   CALL      'TRUL19R'
029400020130     C                   PARM      '15'          CODTAB            2
029500020305     C                   PARM      '1'           ORDINA            1
029600020130     C                   PARM                    KEYTAB            8
029700020130     C                   PARM                    CMD               1
029800941019     C                   MOVEL     '1'           WEMFOR            1
029900020130     C                   MOVEL     KEYTAB        WNAR
030000970701     C                   MOVEL     '6'           O14ERR
030100950116     C                   GOTO      ENDNAZ
030200941019    1C                   ENDIF
030300941019     C* CONTROLLO
030400941019     C                   MOVEL(P)  WNAR          KEY
030500941019     C*
030600941019     C     KTAB          CHAIN     TABEL                              30
030700941019     C  N30TBLFLG        IFNE      ' '
030800941019     C                   SETON                                        30
030900941019     C                   END
031000941019     C*
031100941019    1C     *IN30         IFEQ      *ON
031200970701     C                   MOVEL     ERR(6)        O14MSG
031300970701     C                   MOVEL     '6'           O14ERR
031400950116     C                   GOTO      ENDNAZ
031500941019    1C                   ENDIF
031600941019     C*
031700941019     C                   MOVEL     TBLUNI        DS15
031800941123     C*
031900941123     C     §15ITA        IFEQ      'I'
032000941123     C                   CLEAR                   WNAR
032100941123     C                   ENDIF
032200950116     C     ENDNAZ        TAG
032300950116     C*
032400970701     C     O14ERR        IFNE      *BLANKS
032500950116     C                   MOVEL     '1'           WEMFOR
032600950116    1C                   END
032700950116     C                   ENDSR
032800941019     C*
032900950116     C* CONTROLLO P.IVA ----------------------------------------------*
033000950116     C     CTRPIV        BEGSR
033100950116     C*
033200950116     C                   CLEAR                   WEMFOR
033300950116     C*
033400941019     C* PARTITA IVA
033500941019    1C     WPIVA         IFNE      *BLANKS
033600941019     C* PROGRAMMA CHE CONTROLLA LA P.IVA
033700061107     c                   clear                   xcfivads
033800061107     c                   eval      xcfivamod = 'P'
033900061107     c                   eval      xcfivapar = wpiva
034000061107     c                   eval      xcfivanar = wnar
034100061107     c                   eval      xcfivaprv = e14prv
034200061107     c                   call      'XCFIVAR'
034300061107     c                   parm                    xcfivads
034400100115     c                   parm                    i14forzait
034500100115     c
034600061107     **C                   CLEAR                   XIVAEU
034700061107     **c                   Clear                   xivaprv           2
034800061107     **C*
034900061107     **C                   MOVEL     WNAR          IVANAR
035000061107     **C                   MOVEL     WCPI          IVAPAR
035100061107     **C                   MOVEL     WISO          IVASTA
035200061107     **c                   Movel     e14prv        xivaprv
035300061107     **C*
035400061107     **C                   CALL      'XIVAEUR'
035500061107     **C                   PARM                    XIVAEU
035600061107     **c                   Parm                    xivaprv
035700950613     C* SE SI TRATTA DI UN PRIVATO NON C'E' LA OBBLIGATORIETA
035800950613     C*  NEMMENO PER L'ESTERO
035900950613    2C     WISO          IFNE      '$$'
036000950116     C*
036100950116     C* SE NON IMMESSO CODICE ISO E IMPOSTATO AUTOMATICAMENTE DALLA
036200950116     C*  NAZIONE --> LO EVIDENZIO
036300950613    3C     IVASTA        IFNE      WISO
036400950116     C                   MOVEL     IVASTA        WISO
036500970701     C                   MOVEL     '8'           O14ERR
036600950613    3C                   ENDIF
036700950616    2C                   ENDIF
036800941019     C*
036900061107    3**C     IVAFLG        IFLT      0
037000061108      * frozabile
037100061108     c                   if        xcfivaflg = -9 and wforzaiva = *off and
037200061108     c                             wiva <> e14pip
037300061108     c                   eval      o14err = '7'
037400061108     c                   eval      o14msg = xcfivamsg
037500061108     c                   eval      o14msg = %trim(o14msg) + ' Enter x forzare'
037600061108     c                   eval      wforzaiva = *on
037700061108     c                   eval      wiva = e14pip
037800061108     c                   goto      endcna
037900061108     c                   endif
038000061108      * bloccante
038100061108     c                   if        xcfivaflg < 0 and xcfivaflg <> -9
038200061107     C                   MOVEL     xcfIVAMSG     O14MSG
038300970701     C                   MOVEL     '7'           O14ERR
038400941019     C                   GOTO      ENDCNA
038500950613    3C                   ENDIF
038600941019     C*
038700950613     C*
038800950613   X1C**                   ELSE
038900950613     C* SE ESTERO OBBLIGATORIO SE C'E' ATTRAVERSAMENTO DOGANA
039000970701    2C**         O14IE     IFEQ 'E'
039100970701     C**         O14EFT    ANDNE*BLANKS
039200970701     C**                   MOVELERR,5     O14MSG
039300970701     C**                   MOVEL'7'       O14ERR
039400950613    2C**                   ENDIF
039500950613    1C                   ENDIF
039600941019     C*
039700941019     C     ENDCNA        TAG
039800941019     C*
039900970701     C     O14ERR        IFNE      *BLANKS
040000941019     C                   MOVEL     '1'           WEMFOR
040100941019    1C                   END
040200941019     C                   ENDSR
040300020213**
040400941019Manca Ragione Sociale
040500941019Manca Indirizzo
040600941019Manca localita'
040700941019Manca provincia
040800950613Partita IVA obbligatoria se bolla diretta all'estero con attraversam.dogana
040900941019Nazione Errata
041000960108La ragione sociale contiene il carattere  x  non valido!!
041100110713Inoltro errato  o non utilizzabile                                             8
041200970703C.A.P. incongruente con provincia e localita'                                  9
041300970702C.A.P. inesistente                                                            10
041400020204Codice Stato inesistente o annullato
041500960108**
041600020213§!:@
