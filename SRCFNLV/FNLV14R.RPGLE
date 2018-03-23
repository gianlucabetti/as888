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
002100150825     D tisi30ds      E DS
002200020204     D*
002300020204     D* DS PER TIBS02R - CONTROLLO TABELLE
002400020204     D TIBS02        E DS                  EXTNAME(TIBS02DS)
002500941019     D*
002600941019     D* DS PER DATI IN ENTRATA
002700970701     D DSLV14        E DS                  EXTNAME(FNLV14DS)
002800061030     d* Per passaggio dati di output
002900061030     D ds15x         E DS                  EXTNAME(ds15)
003000061030     d                                     prefix(X_)
003100110713     D ds9a          E DS
003200941019     D*
003300940919     D DS15          E DS
003400000121     D  W15LAD                56     58
003500020124     D  W15LAF                62     64
003600061107
003700151104     d xcfiva1ds     e ds
003800061107     d  ivasta                 3      4
003900061107     d  ivapar                 5     18
004000941019     D*
004100941019     D KPJBA         E DS
004200941019     D                 DS
004300941221     D  WISO                   1      2
004400941221     D  WCPI                   3     16
004500941019     D  WPIVA                  1     16
004600020204     D PARAM1          DS
004700020204     D  P01NZD                 1      3
004800020214     D  P01PRV                 4      5
004900020214     D  P01ORD                 6      6
005000921021     C*---------------------------------------------------------------*
005100921021     C     *ENTRY        PLIST
005200970703     C                   PARM                    KPJBA
005300970703     C                   PARM                    DSLV14
005400061030     C                   PARM                    DS15x
005500000000     C*---------------------------------------------------------------*
005600921015     C     KTAB          KLIST
005700940218     C                   KFLD                    CODUT             1 0
005800921015     C                   KFLD                    COD               2
005900921015     C                   KFLD                    KEY               8
006000940919     C*
006100940919     C                   Z-ADD     1             CODUT
006200970701     C     *LIKE         DEFINE    E14NAR        WNAR
006300941019     C     *LIKE         DEFINE    TBLKEY        §KEY
006400970702     C     *LIKE         DEFINE    E14LOC        SAVLOC
006500940218     C*---------------------------------------------------------------*
006600970701     C* TIPO LANCIO, I14TLA "C" -           CHIUSO CON LR
006700970701     C* TIPO LANCIO, I14TLA "L" - ELABORO E CHIUDO CON LR
006800970701     C* TIPO LANCIO, I14TLA " " - ELABORO E CHIUDO IN RETRN
006900970701    1C     I14TLA        IFNE      'C'
007000940921     C*
007100970701     C                   CLEAR                   O14MSG
007200970701     C                   CLEAR                   O14ERR
007300061030     c                   if        %parms=3
007400061030     c                   clear                   ds15x
007500061030     c                   endif
007600061108      * se p.iva o ragione sociale diversi pulisco il flag di forzatura
007700061108     c                   if        wiva <> e14pip or wrag <> i14rsc
007800061108     c                   eval      wforzaiva = *off
007900061108     c                   endif
008000061108      * salvo ragione sociale
008100061108     c                   if        wrag = *blanks
008200061108     c                   eval      wrag = i14rsc
008300061108     c                   endif
008400941019     C*
008500941019     C* RAGIONE SOCIALE
008600970701     C     I14RSC        IFEQ      *BLANKS
008700970701     C     I14RS2        ANDEQ     *BLANKS
008800970701     C                   MOVEL     '1'           O14ERR
008900970701     C                   MOVEL     ERR(1)        O14MSG
009000941019     C                   GOTO      ENDC01
009100941019     C                   ENDIF
009200960108     C*
009300941025     C* SE IMMESSA SOLO LA 2 E NON LA 1 --> ERRORE
009400970701     C     I14RSC        IFEQ      *BLANKS
009500970701     C     I14RS2        ANDNE     *BLANKS
009600970701     C                   MOVEL     '1'           O14ERR
009700970701     C                   MOVEL     ERR(1)        O14MSG
009800941025     C                   GOTO      ENDC01
009900941025     C                   ENDIF
010000960108     C*
010100960108     C* CONTROLLO SE LA RAGIONE SOCIALE CONTIENE DEI CARATTERI NON
010200960108     C*  UTILIZZABILI
010300960108     C                   Z-ADD     1             X                 2 0
010400960108     C*
010500110920     C*    X             DOWLE     10
010600110920     C*    CAR(X)        ANDNE     *BLANKS
010700110920     C*    CAR(X)        SCAN      I14RSC                                 31
010800110920     C* N31CAR(X)        SCAN      I14RS2                                 31
010900110920     C*    *IN31         IFEQ      *ON
011000110920     C*                  MOVEA     ERR(7)        K78
011100110920     C*                  MOVEL     CAR(X)        K78(43)
011200110920     C*                  MOVEA     K78           O14MSG
011300110920     C*                  MOVEL     '1'           O14ERR
011400110920     C*                  Z-ADD     10            X
011500110920     C*                  ENDIF
011600110920     C*                  ADD       1             X
011700110920     C*                  ENDDO
011800960108     C*
011900110920     C*  31              GOTO      ENDC01
012000941019     C* INDIRIZZO
012100970701     C     I14IND        IFEQ      *BLANKS
012200970701     C                   MOVEL     '2'           O14ERR
012300970701     C                   MOVEL     ERR(2)        O14MSG
012400941019     C                   GOTO      ENDC01
012500941019     C                   ENDIF
012600941019     C* LOCALITA
012700970707     C     E14LOC        IFEQ      *BLANKS
012800970707     C                   MOVEL     '3'           O14ERR
012900970707     C                   MOVEL     ERR(3)        O14MSG
013000970707     C                   GOTO      ENDC01
013100970707     C                   END
013200950116     C* NAZIONE
013300970701     C                   MOVEL     E14NAR        WNAR
013400941019     C                   EXSR      CTRNAR
013500970701     C                   MOVEL     WNAR          E14NAR
013600941019     C     WEMFOR        IFEQ      '1'
013700941019     C                   GOTO      ENDC01
013800941019     C                   ENDIF
013900970516     c*
014000970516     C* CONTROLLO SE IMMESSO FLAG INOLTRO
014100970702     C     I14ISO        IFNE      ' '
014200970516     C* CONTROLLO
014300970723     C                   MOVEL     '9A'          COD
014400970702     C                   MOVEL(P)  I14ISO        KEY
014500970516     C*
014600970516     C     KTAB          CHAIN     TABEL                              30
014700970516     C  N30TBLFLG        IFNE      ' '
014800970516     C                   SETON                                        30
014900970516     C                   END
015000970516     C*
015100970516    1C     *IN30         IFEQ      *ON
015200970701     C                   MOVEL     ERR(8)        O14MSG
015300970701     C                   MOVEL     '9'           O14ERR
015400970516     C                   GOTO      ENDC01
015500970516    1C                   ENDIF
015600110713     c
015700110713     c                   movel     tbluni        ds9a
015800110713     c* Inoltro non utilizzabile nei pgm operativi
015900110713     c                   if        §9autiop='N'
016000110713     C                   MOVEL     ERR(8)        O14MSG
016100110713     C                   MOVEL     '9'           O14ERR
016200110713     C                   GOTO      ENDC01
016300110713    1C                   ENDIF
016400970516     C                   ENDIF
016500941019     C*
016600941019     C* MEMORIZZO SE DESTINATARIO ITALIA O ESTERO
016700941019     C     §15ITA        IFNE      'I'
016800970701     C                   MOVEL     'E'           O14IE
016900941019     C                   ELSE
017000970701     C                   MOVEL     'I'           O14IE
017100941019     C                   ENDIF
017200941019     C* MEMORIZZO DIVISA DEL DESTINATARIO CHE MI PUO' SERVIRE X IL C/A
017300970701     C                   MOVEL     §15VLT        O14VLT
017400970701     C                   MOVEL     §15EFT        O14EFT
017500000321     C                   MOVEL     §15EFD        O14EFD
017600020124     C                   MOVEL     §15EFF        O14EFF
017700971215     C                   MOVEL     §15COD        O14COD
017800980219     C                   MOVEL     §15CPP        O14CPP
017900060921     c                   movel     §15red        o14red
018000000121     C     W15LAD        IFGT      *ZEROS
018100000121     C                   MOVEL     W15LAD        O14LAD
018200060919     c* memorizzo flag se vale solo per reso
018300000121     C                   ELSE
018400000121     C                   CLEAR                   O14LAD
018500000121     C                   ENDIF
018600020124     C*
018700020124     C     W15LAF        IFGT      *ZEROS
018800020124     C                   MOVEL     W15LAF        O14LAF
018900020124     C                   ELSE
019000020124     C                   CLEAR                   O14LAF
019100020124     C                   ENDIF
019200020130     C                   MOVEL     §15FFN        O14FFN
019300020206     C                   MOVEL     §15EEX        O14EEX
019400061030     c
019500061030     c* Se passata 3°parms, imposto la DS15X
019600061030     c                   if        %parms=3
019700061030     c                   movel     ds15          ds15x
019800061030     c                   endif
019801161109     c* Se richiamo BATCH errore se non passato cap e provincia
019802161109     c                   if        i14batch='S' and
019803161109     c                             e14cap=*blanks and e14prv=*blanks
019804161109     C                   MOVEL     ERR(4)        O14MSG
019805161109     C                   MOVEL     '5'           O14ERR
019806161109     C                   GOTO      ENDC01
019807161109    1C                   ENDIF
019900941129     C*
020000960327     C* CAP VUOTO  O CON '?' : INTERROGAZIONE SU CAPPARIO
020100161109     C* SOLO SE NAZIONE CON CAPPARIO  e richiamo interattivo
020200161109    1C     O14CPP        IFEQ      *BLANKS
020201161109
020202161109   1ac     i14batch      ifeq      ' '
020300970701     C     '?'           SCAN      E14CAP                                 30
020400960327     C*
020500161109    2C     E14CAP        IFEQ      *BLANKS
020600970701     C     E14PRV        ANDEQ     *BLANKS
020700960327     C     *IN30         OREQ      *ON
020800970701     C                   CLEAR                   DSSI00
020900970703     C                   MOVEL     'L01'         D00OP0
021000970701     C                   MOVEL     E14LOC        D00LOC
021100970701     C                   MOVEL     I14DRI        D00DRI
021200980220     C                   MOVEL     E14NAR        D00NAR
021300150825     c                   clear                   tisi30ds
021400150825     c                   movel     'S'           i30flg
021500150825     c                   eval      I30NAR=e14nar
021600150825     c                   eval      I30CAP=e14cap
021700150825     c                   eval      I30LOC=e14loc
021800150825     c                   eval      I30PRV=e14prv
021900150825
022000970701     C                   CALL      'TISI30R'
022100941129     C                   PARM                    KPJBA
022200970701     C                   PARM                    DSSI00
022300150825     c                   parm                    tisi30ds
022400970701     C* SE SELEZIONATO CAP
022500161109    3C     D00CAP        IFNE      *BLANKS
022600970701     C                   MOVEL     D00CAP        E14CAP
022700970701     C                   MOVEL     D00LOC        E14LOC
022800970701     C                   MOVEL     D00NAR        E14NAR
022900970704     C                   MOVEL     D00PRV        E14PRV
023000161109    3C                   END
023100970704     C**
023200970701     C                   MOVEL     '5'           O14ERR
023300941129     C                   GOTO      ENDC01
023400941129     C*
023500161109    2C                   END
023501161109   1aC                   endif
023600161109   x1C                   ELSE
023700020204     C* NAZIONE SENZA CAPPARIO
023800020204     C* GESTISCO OBBLIGATORIETA' DELLO STATO
023900020205     C     I14CNZ        IFEQ      'S'
024000020205    2C     §15STO        ANDEQ     'S'
024001161109
024100020204     C* SE NON IMMESSO RICHIAMO PGM DI INTERROGAZIONE/SCELTA
024101161109     c*   solo se interattivo
024200020204    3C     E14PRV        IFEQ      *BLANKS
024201161109     c     i14batch      andeq     ' '
024300020204     C                   MOVE      KPJBU         WA256           256
024400020204     C                   CLEAR                   PARAM1
024500020204     C                   MOVEL     E14NAR        P01NZD
024600020204     C                   MOVEL     '1'           P01ORD
024700020204     C                   MOVEL     PARAM1        KPJBU
024800020204     C                   CALL      'TNTB20R'
024900020204     C                   PARM                    KPJBA
025000020204     C                   MOVEL     KPJBU         PARAM1
025100020204     C                   MOVEL     P01PRV        E14PRV
025200020204     C                   MOVE      WA256         KPJBU
025300020204     C                   MOVEL     '0'           O14ERR
025400020204     C                   GOTO      ENDC01
025500020204   X3C                   ELSE
025600020204     C* ALTRIMENTI DEVE ESISTERE SU TABELLA STA
025700020204     C                   CLEAR                   TIBS02
025800020204     C                   MOVEL     'C'           T02MOD
025900020204     C                   MOVEL     'STA'         T02COD
026000020204     C                   MOVEL(P)  E14NAR        T02KE1
026100020204     C                   MOVEL(P)  E14PRV        T02KE2
026200020204     C                   MOVEL     KNSIF         T02SIF
026300020204     C                   CALL      'TIBS02R'
026400020204     C                   PARM                    KPJBA
026500020204     C                   PARM                    TIBS02
026600020204     C     T02ERR        IFEQ      'E'
026700020204     C     T02ERR        OREQ      'A'
026800020204     C                   MOVEL     '0'           O14ERR
026900020204     C                   MOVEL     ERR(11)       O14MSG
027000020204     C                   GOTO      ENDC01
027100020204     C                   ENDIF
027200020204    3C                   ENDIF
027300020204    2C                   ENDIF
027400161109    1C                   END
027500941020     C*
027600950116     C* PARTITA IVA
027700970701     C                   MOVEL     E14PIP        WPIVA
027800950116     C                   EXSR      CTRPIV
027900970701     C                   MOVEL     WPIVA         E14PIP
028000950116     C     WEMFOR        IFEQ      '1'
028100950116     C                   GOTO      ENDC01
028200950116     C                   ENDIF
028300941019     C                   ENDIF
028400941019     C*
028500941019     C     ENDC01        TAG
028600941019     C* CHIUSURA PGM
028700970701    1C     I14TLA        IFEQ      ' '
028800941019     C                   RETURN
028900941019     C                   ELSE
029000941019     C                   SETON                                        LR
029100941019    1C                   ENDIF
029200940919     C*
029300950116     C* CONTROLLO NAZIONE --------------------------------------------*
029400941019     C     CTRNAR        BEGSR
029500941129     C*
029600941129     C                   CLEAR                   WEMFOR
029700941019     C*
029800941019     C                   MOVEL     '15'          COD
029900161109     C* RICERCA solo interattivo
030000941019     C     '?'           SCAN      WNAR                                   30
030100941019    1C     *IN30         IFEQ      *ON
030101161109     c     i14batch      andeq     ' '
030102161109
030200020130     C                   CALL      'TRUL19R'
030300020130     C                   PARM      '15'          CODTAB            2
030400020305     C                   PARM      '1'           ORDINA            1
030500020130     C                   PARM                    KEYTAB            8
030600020130     C                   PARM                    CMD               1
030700941019     C                   MOVEL     '1'           WEMFOR            1
030800020130     C                   MOVEL     KEYTAB        WNAR
030900970701     C                   MOVEL     '6'           O14ERR
031000950116     C                   GOTO      ENDNAZ
031100941019    1C                   ENDIF
031200941019     C* CONTROLLO
031300941019     C                   MOVEL(P)  WNAR          KEY
031400941019     C*
031500941019     C     KTAB          CHAIN     TABEL                              30
031600941019     C  N30TBLFLG        IFNE      ' '
031700941019     C                   SETON                                        30
031800941019     C                   END
031900941019     C*
032000941019    1C     *IN30         IFEQ      *ON
032100970701     C                   MOVEL     ERR(6)        O14MSG
032200970701     C                   MOVEL     '6'           O14ERR
032300950116     C                   GOTO      ENDNAZ
032400941019    1C                   ENDIF
032500941019     C*
032600941019     C                   MOVEL     TBLUNI        DS15
032700941123     C*
032800941123     C     §15ITA        IFEQ      'I'
032900941123     C                   CLEAR                   WNAR
033000941123     C                   ENDIF
033100950116     C     ENDNAZ        TAG
033200950116     C*
033300970701     C     O14ERR        IFNE      *BLANKS
033400950116     C                   MOVEL     '1'           WEMFOR
033500950116    1C                   END
033600950116     C                   ENDSR
033700941019     C*
033800950116     C* CONTROLLO P.IVA ----------------------------------------------*
033900950116     C     CTRPIV        BEGSR
034000950116     C*
034100950116     C                   CLEAR                   WEMFOR
034200950116     C*
034300941019     C* PARTITA IVA
034400941019    1C     WPIVA         IFNE      *BLANKS
034500941019     C* PROGRAMMA CHE CONTROLLA LA P.IVA
034600151104     c                   clear                   xcfiva1ds
034700061107     c                   eval      xcfivamod = 'P'
034800061107     c                   eval      xcfivapar = wpiva
034900061107     c                   eval      xcfivanar = wnar
035000061107     c                   eval      xcfivaprv = e14prv
035100151104     c                   eval      xcfivacap = e14cap
035200151104     c                   eval      xcfivaloc = e14loc
035300151104     c                   eval      XCFIVAFORI= i14forzait
035400151104     c                   eval      XCFIVAP88 = i14p88
035500151104     c                   call      'XCFIVAR1'
035600151104     c                   parm                    xcfiva1ds
035700100115     c
035800950613     C* SE SI TRATTA DI UN PRIVATO NON C'E' LA OBBLIGATORIETA
035900950613     C*  NEMMENO PER L'ESTERO
036000950613    2C     WISO          IFNE      '$$'
036100950116     C*
036200950116     C* SE NON IMMESSO CODICE ISO E IMPOSTATO AUTOMATICAMENTE DALLA
036300950116     C*  NAZIONE --> LO EVIDENZIO
036400950613    3C     IVASTA        IFNE      WISO
036500950116     C                   MOVEL     IVASTA        WISO
036600970701     C                   MOVEL     '8'           O14ERR
036700950613    3C                   ENDIF
036800950616    2C                   ENDIF
036900941019     C*
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
039800161109Dati mancanti:  provincia e CAP
039900950613Partita IVA obbligatoria se bolla diretta all'estero con attraversam.dogana
040000941019Nazione Errata
040100960108La ragione sociale contiene il carattere  x  non valido!!
040200110713Inoltro errato  o non utilizzabile                                             8
040300970703C.A.P. incongruente con provincia e localita'                                  9
040400970702C.A.P. inesistente                                                            10
040500020204Codice Stato inesistente o annullato
040600960108**
040700020213§!:@
