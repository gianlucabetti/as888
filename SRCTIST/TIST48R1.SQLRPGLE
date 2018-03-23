000100111129     /*PRM dbgview(*source)
000200111129     /*END
000300000000     H*------------------------------------------------------------------------*
000400060901     H* CONTROLLO ASSORBIMENTO X LNA
000500000000     H*------------------------------------------------------------------------*
000600000000     H DECEDIT('0,') DATEDIT(*DMY.)
000700111129     h option(*nodebugio)
000800111129     h dftactgrp(*no)
000900111129     h alwnull(*inputonly)
001000000914     H*------------------------------------------------------------------------*
001100000000     FAZORG01L  IF   E           K DISK
001200001016     FTABEL00F  IF   E           K DISK
001300061030     FAZCPL01L  IF   E           K DISK
001400111129     F*//TIALA01L  IF   E           K DISK
001500111129     F*//TIALA02L  IF   E           K DISK    rename(tiala000:tiala002)
001600060901     FTIST48P   O    E             PRINTER OFLIND(*IN01)
001700000000     D*------------------------------------------------------------------------*
001800000000     D* SCHIERE
001900000000     D*------------------------------------------------------------------------*
002000000000     D*-------------------
002100060904     D* FILIALI E RELATIVE DESCRIZIONI
002200000000     D*-------------------
002300000915     D FIL             S              3  0 DIM(500)                             *FILIALE
002400001106     D DFIL            S             20    DIM(500)                             *DESCRIZIONE FILIALE
002500000000     D*------------------------------------------------------------------------*
002600000000     D* INPUT
002700000000     D*------------------------------------------------------------------------*
002800000000     D*-------------------
002900000000     D* ARCHITETTURA
003000000000     D*-------------------
003100000000     D KPJBA         E DS
003200060901     D*-------------------
003300060901     D* DS PARAMETRI DI LANCIO PGM
003400060901     D*-------------------
003500060901     D PARAM           DS                  INZ
003600060901     D  PAROPE                        1
003700060901     D  PARDAI                        8  0
003800060901     D  PARDAF                        8  0
003900060901     D  PARIAD                        8  0
004000060901     D  PARFAD                        8  0
004100060901     D  PARGIO                        3  0
004200060901     D  PARDET                        1
004300061027     D  PARORD                        1
004400060901     D  PARKG1DA                      7  1
004500060901     D  PARKG1A                       7  1
004600060901     D  PARKG2DA                      7  1
004700060901     D  PARKG2A                       7  1
004800060901     D  PARKG3DA                      7  1
004900060901     D  PARKG3A                       7  1
005000060901     D  PARKG4DA                      7  1
005100060901     D  PARKG4A                       7  1
005200060901     D  PARDIS                        1
005300060901     D  PARARE                        3  0
005400060901     D  PARFIL                        3  0
005500060901     D  PARCAD                        2
005600060901     D  PARCAA                        2
005700111129       //
005800111129       // -?Dati letti via SQL?
005900111129     d ds_TIALA      e ds                  extname(TIALA00F)  inz
006000111129      *
006100061030     D*------------------
006200061030     D* DS "XSRDA8" - CONTROLLA DATA (8)
006300061030     D*------------------
006400061030     D WLBDA8          DS                  INZ
006500061030     D  G08DAT                 1      8  0
006600061030     D  G08INV                 9     16  0
006700061030     D  G08ERR                17     17
006800061030     D  G08TGI                18     22  0
006900000914     D*------------------------------------------------------------------------*
007000060904     D* VARIABILI DI WRK
007100000914     D*------------------------------------------------------------------------*
007200111129     D*// tialads_key   e ds                  extname(tiala00f) prefix(k_) inz
007300060904     D depspv          s              7S 1 inz
007400111129     d wCountSel       s              3  0 inz
007500111129
007600111129       // -?Stringhe SQL da eseguire?
007700111129     d wSql            s           5000    inz  varying
007800060904     D*-------------------
007900060904     D* Variabili riferite al data base
008000060904     D*-------------------
008100060901     D depdiv          S                   LIKE(aladiv)
008200060901     D depare          S                   LIKE(alaare)
008300060904     D depfil          S                   LIKE(alalna)
008400060904     D depznc          S                   LIKE(alaznc)
008500060904     D depnaz          S                   LIKE(alanaz)
008600060901     D depprv          S                   LIKE(alaprv)
008700060901     D depcap          S                   LIKE(alacap)
008800060901     D deploc          S                   LIKE(alaloc)
008900061030     D depfi1          S                   LIKE(cplfi1)
009000111129
009100111129       //--------------------------------------------------------------
009200111129       //?Definizione aree dati.                                       ?
009300111129       //--------------------------------------------------------------
009400111129
009500111129       // -?Dati utente?
009600111129     d §AzUte        e ds                  extname(AZUTE00F)
009700111129     d                                     dtaara
009800111129     d §DatiUte      e ds                  extname(dDatiUte)
009900111129     d                                     dtaara
010000111129
010100111129       //--------------------------------------------------------------
010200111129       //?Definizione prototipi procedure.                             ?
010300111129       //--------------------------------------------------------------
010400111129
010500111129       // -?Reperimento dati utente?
010600111129     d TIBS34ds      e ds                  inz
010700111129      /copy gaitrasrc/srcProtoPr,TIBS34R
010800111129
010900111129       // -?Parametri API QCAPCMD (Process Commands)?
011000111129     d Qcmd            s           2048    inz  varying
011100111129      /copy qSysInc/qRpgleSrc,QCAPCMD
011200111129       // -?API QCAPCMD (Process Commands)?
011300111129      /copy gaitrasrc/srcProtoPR,QCAPCMD
011400111129
011500111129       // -?Parametri gestione errori API.?
011600111129      /copy qsysinc/qrpglesrc,QUSEC
011700111129
011800000000     C*------------------------------------------------------------------------*
011900000000     C* MAIN LINE
012000000000     C*------------------------------------------------------------------------*
012100060904     C*
012200060904     C* Caricamento organigramma
012300060904     C                   exsr      carorg
012400000000     C*
012500060904     C* Eseguo stampa
012600001106     C                   exsr      stampa
012700000914     C*
012800000914     C                   SETON                                        LR
012900001106     C*------------------------------------------------------------------------*
013000001106     C* STAMPA - Routine di stampa
013100001106     C*------------------------------------------------------------------------*
013200001106     C     STAMPA        BEGSR
013300001207     C*
013400001207     C* Inizializzo la testata
013500060901     C                   clear                   ST48TE
013600111130     c                   eval      DSFIRS = RSUt
013700001109     C*
013800001109     C* Imposto i parametri in testata
013900060901     C                   z-add     PARKG1A       STPPP1A
014000060901     C                   z-add     PARKG1DA      STPPP1D
014100060901     C                   z-add     PARKG2A       STPPP2A
014200060901     C                   z-add     PARKG2DA      STPPP2D
014300060901     C                   z-add     PARKG3A       STPPP3A
014400060901     C                   z-add     PARKG3DA      STPPP3D
014500060901     C                   z-add     PARKG4A       STPPP4A
014600060901     C                   z-add     PARKG4DA      STPPP4D
014700060904     C                   z-add     PARIAD        STPPDAI
014800060904     C                   z-add     PARFAD        STPPDAF
014900060904     C                   z-add     PARGIO        STPPGIO
015000020312     C*
015100001109     C                   if        PARCAD = *blanks
015200060904     C                   movel     *zeros        STPPCAD
015300001109     C                   else
015400060904     C                   movel     PARCAD        STPPCAD
015500001109     C                   endif
015600060904     C*
015700001109     C                   if        PARCAA = *blanks
015800060904     C                   movel     *all'9'       STPPCAA
015900001109     C                   else
016000060904     C                   movel     PARCAA        STPPCAA
016100001109     C                   endif
016200061030     C
016300061030     C* Indicazione tipo ordinamento stampa
016400061030     C                   if        PARORD = 'C'
016500061030     C                   eval      STPTIT = '          (Ordinamento per CAP)'
016600061030     C                   else
016700061030     C                   eval      STPTIT = '         (Ordinamento per ZONA)'
016800061030     C                   endif
016900060904     C
017000070410     C* Gestire valori e decodifiche DIVISIONE / AREA / FIL.
017100060904     C                   movel     PARDIS        STPPDIS
017200060904     C                   if        STPPDIS = *blanks
017300060904     C                   eval      STPPDISD = 'Tutti'
017400060904     C                   else
017500060904     C                   movel     STPPDIS       depdiv
017600060904     C                   exsr      DECDIV
017700060904     C                   eval      STPPDISD = desdiv
017800060904     C                   endif
017900060904     C*
018000060904     C                   z-add     PARARE        STPPARE
018100060904     C                   if        STPPARE = *zeros
018200060904     C                   eval      STPPARED = 'Tutte'
018300060904     C                   else
018400060904     C                   movel     STPPARE       depare
018500060904     C                   exsr      DECARE
018600060904     C                   eval      STPPARED = desare
018700060904     C                   endif
018800060904     C*
018900060904     C                   z-add     PARFIL        STPPFIL
019000060904     C                   if        STPPFIL = *zeros
019100111130     C                   eval      STPPFILD = 'Tutte'
019200060904     C                   else
019300060904     C                   movel     STPPFIL       depfil
019400060904     C                   exsr      DECFIL
019500060904     C                   eval      STPPFILD = desfil
019600060904     C                   endif
019700060904     C*
019800060904     C* Inizializzo le variabili d deposito x le rotture d codice in stampa
019900060904     C                   clear                   depdiv
020000060904     C                   clear                   depare
020100060904     C                   clear                   depfil
020200060904     C                   clear                   depznc
020300060904     C                   clear                   depnaz
020400060904     C                   clear                   depprv
020500060904     C                   clear                   depcap
020600060904     C                   clear                   deploc
020700061030     C                   clear                   depfi1
020800061027     C*
020900061027     C* Verifico il tipo d ordinamento richiesto
021000061027     C                   IF        PARORD = 'C'
021100061027     C                   SETON                                        10
021200061027     C                   ELSE
021300061027     C                   SETOFF                                       10
021400061027     C                   ENDIF
021500070117     C*
021600070117     C* Posizionamento sul file di wrk e prima lettura
021700070117     C                   exsr      setfil
021800061027     C*
021900061027     C* Elaborazione x ordionamento richiesto
022000061027     C   10              exsr      elaOrd_CL
022100061027     C  N10              exsr      elaOrd_ZCL
022200001106     C*
022300001106     C* Stampa il totale GENERALE
022400001109     C                   exsr      stagen
022500001106     C*
022600001106     C                   ENDSR
022700061027     C*------------------------------------------------------------------------*
022800061027     C* ELAORD_CL - ELABORAZIONE ORDINATA X CAP/LOC
022900061027     C*------------------------------------------------------------------------*
023000061027     C     ELAORD_CL     BEGSR
023100061027     C*
023200061027     C* Ciclo fino a fine file di wrk
023300061027     C     finl00        doweq     '0'
023400061027     C*
023500061027     C* Operazioni per nuova DIVISIONE
023600061027     C                   exsr      newdiv
023700061027     C*
023800061027     C* Ciclo fino a rottura divisione
023900061027     C     finl00        doweq     '0'
024000061027     C     aladiv        andeq     depdiv
024100061027     C*
024200061027     C* Operazioni per nuova AREA
024300061027     C                   exsr      neware
024400061027     C*
024500061027     C* Ciclo fino a rottura di AREA
024600061027     C     finl00        doweq     '0'
024700061027     C     aladiv        andeq     depdiv
024800061027     C     alaare        andeq     depare
024900061027     C*
025000061027     C* Operazioni per nuova FILIALE
025100061027     C                   exsr      newfil
025200061027     C*
025300061027     C* Ciclo fino a rottura di FILIALE
025400061027     C     finl00        doweq     '0'
025500061027     C     aladiv        andeq     depdiv
025600061027     C     alaare        andeq     depare
025700061027     C     alalna        andeq     depfil
025800061027     C*
025900061027     C* Operazioni per nuovo PRV/CAP
026000061027     C                   exsr      newcap
026100061027     C*
026200061027     C* Ciclo fino a rottura di PRV/CAP
026300061027     C     finl00        doweq     '0'
026400061027     C     aladiv        andeq     depdiv
026500061027     C     alaare        andeq     depare
026600061027     C     alalna        andeq     depfil
026700061027     C     alanaz        andeq     depnaz
026800061027     C     alaprv        andeq     depprv
026900061027     C     alacap        andeq     depcap
027000061027     C*
027100061027     C* Operazioni per nuovo PRV/CAP/LOC
027200061027     C                   exsr      newdet_c
027300061027     C*
027400061027     C* Ciclo fino a rottura di PRV/CAP/LOC
027500061027     C     finl00        doweq     '0'
027600061027     C     aladiv        andeq     depdiv
027700061027     C     alaare        andeq     depare
027800061027     C     alalna        andeq     depfil
027900061027     C     alanaz        andeq     depnaz
028000061027     C     alaprv        andeq     depprv
028100061027     C     alacap        andeq     depcap
028200061027     C     alaloc        andeq     deploc
028300061027     C*
028400061027     C* Memorizza i dati di DETTAGLIO
028500061027     C     WRECOK        ifeq      'S'
028600061027     C                   exsr      memspe
028700061027     C                   endif
028800061027     C*
028900061027     C* Legge il record successivo
029000061027     C                   exsr      letfil
029100061027     C*
029200061027     C                   enddo
029300061027     C*
029400061027     C* Stampa i dati di DETTAGLIO
029500061027     C     PARDET        ifeq      'S'
029600061027     C                   exsr      stadet
029700061027     C                   endif
029800061027     C*
029900061027     C                   enddo
030000061027     C*
030100061027     C* Stampa il totale CAP
030200061027     C                   exsr      stacap
030300061027     C*
030400061027     C                   enddo
030500061027     C*
030600061027     C* Stampa il totale FILIALE
030700061027     C                   exsr      stafil
030800061027     C*
030900061027     C                   enddo
031000061027     C*
031100061027     C* Stampa il totale AREA
031200061027     C                   exsr      staare
031300061027     C*
031400061027     C                   enddo
031500061027     C*
031600061027     C* Stampa il totale DIVISIONE
031700061027     C                   exsr      stadiv
031800061027     C*
031900061027     C                   enddo
032000061027     C*
032100061027     C                   ENDSR
032200061027     C*------------------------------------------------------------------------*
032300061027     C* ELAORD_ZCL - ELABORAZIONE ORDINATA X ZNC/CAP/LOC
032400061027     C*------------------------------------------------------------------------*
032500061027     C     ELAORD_ZCL    BEGSR
032600061027     C*
032700061027     C* Ciclo fino a fine file di wrk
032800061027     C     finl00        doweq     '0'
032900061027     C*
033000061027     C* Operazioni per nuova DIVISIONE
033100061027     C                   exsr      newdiv
033200061027     C*
033300061027     C* Ciclo fino a rottura divisione
033400061027     C     finl00        doweq     '0'
033500061027     C     aladiv        andeq     depdiv
033600061027     C*
033700061027     C* Operazioni per nuova AREA
033800061027     C                   exsr      neware
033900061027     C*
034000061027     C* Ciclo fino a rottura di AREA
034100061027     C     finl00        doweq     '0'
034200061027     C     aladiv        andeq     depdiv
034300061027     C     alaare        andeq     depare
034400061027     C*
034500061027     C* Operazioni per nuova FILIALE
034600061027     C                   exsr      newfil
034700061027     C*
034800061027     C* Ciclo fino a rottura di FILIALE
034900061027     C     finl00        doweq     '0'
035000061027     C     aladiv        andeq     depdiv
035100061027     C     alaare        andeq     depare
035200061027     C     alalna        andeq     depfil
035300061027     C*
035400061027     C* Operazioni per nuova ZNC
035500061027     C                   exsr      newznc
035600061027     C*
035700061027     C* Ciclo fino a rottura di ZONA CONSEGNA
035800061027     C     finl00        doweq     '0'
035900061027     C     aladiv        andeq     depdiv
036000061027     C     alaare        andeq     depare
036100061027     C     alalna        andeq     depfil
036200061027     C     alaznc        andeq     depznc
036300061027     C*
036400061027     C* Operazioni per buovo PRV/CAP/LOC
036500061027     C                   exsr      newdet_z
036600061027     C*
036700061027     C* Ciclo fino a rottura di PRV/CAP/LOC
036800061027     C     finl00        doweq     '0'
036900061027     C     aladiv        andeq     depdiv
037000061027     C     alaare        andeq     depare
037100061027     C     alalna        andeq     depfil
037200061027     C     alaznc        andeq     depznc
037300061027     C     alanaz        andeq     depnaz
037400061027     C     alaprv        andeq     depprv
037500061027     C     alacap        andeq     depcap
037600061027     C     alaloc        andeq     deploc
037700061027     C*
037800061027     C* Memorizza i dati di DETTAGLIO
037900061027     C     WRECOK        ifeq      'S'
038000061027     C                   exsr      memspe
038100061027     C                   endif
038200061027     C*
038300061027     C* Legge il record successivo
038400061027     C                   exsr      letfil
038500061027     C*
038600061027     C                   enddo
038700061027     C*
038800061027     C* Stampa i dati di DETTAGLIO
038900061027     C     PARDET        ifeq      'S'
039000061027     C                   exsr      stadet
039100061027     C                   endif
039200061027     C*
039300061027     C                   enddo
039400061027     C*
039500061027     C* Stampa il totale ZONA CONSEGNA
039600061027     C                   exsr      staznc
039700061027     C*
039800061027     C                   enddo
039900061027     C*
040000061027     C* Stampa il totale FILIALE
040100061027     C                   exsr      stafil
040200061027     C*
040300061027     C                   enddo
040400061027     C*
040500061027     C* Stampa il totale AREA
040600061027     C                   exsr      staare
040700061027     C*
040800061027     C                   enddo
040900061027     C*
041000061027     C* Stampa il totale DIVISIONE
041100061027     C                   exsr      stadiv
041200061027     C*
041300061027     C                   enddo
041400061027     C*
041500061027     C                   ENDSR
041600001106     C*------------------------------------------------------------------------*
041700001106     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE
041800001106     C*------------------------------------------------------------------------*
041900001106     C     SETFIL        BEGSR
042000001106     C*
042100111129     C*//* Posizionamento
042200111129     C*//                   movel     PARDIS        k_aladiv
042300111129     C*//                   z-add     PARARE        k_alaare
042400111129     C*//                   z-add     PARFIL        k_alalna
042500111129     C*//                   movel     *blanks       k_alanaz
042600111129     C*//                   movel     *blanks       k_alaprv
042700111129     C*//                   movel     *blanks       k_alacap
042800111129     C*//                   movel     *blanks       k_alaloc
042900111129     C*//                   z-add     *zeros        k_alaznc
043000111129     C*//                   movel     *blanks       k_alaspv
043100001113     C*
043200111129     C*//   10KEYala01_c    setll     tiala01l                           99
043300111129     C*//  N10KEYala02_c    setll     tiala02l                           99
043400001106     C*
043500111129     C*//     *in99         ifeq      '1'
043600111129     C*//                   movel     '1'           finl00
043700111129     C*//                   else
043800111129     C*//                   exsr      letfil
043900111129     C*//                   endif
044000111129
044100111129      /free
044200111129
044300111129         // -?Preparazione stringa SQL?
044400111129         clear  wCountSel;
044500111129         wSql  = 'select * +
044600111129                    from TIALA00F';
044700111129
044800111129         // -?Selezione per distretto?
044900111129         if  ParDIS > *blank;
045000111129           if  wCountSel = *zero;
045100111129             wSQL += ' where';
045200111129           else;
045300111129             wSQL += ' and';
045400111129           endif;
045500111130           wSQL += ' ALAdiv = ''' + ParDIS + '''';
045600111129           wCountSel += 1;
045700111129         endif;
045800111129
045900111129         // -?Selezione per area?
046000111129         if  ParARE > *zero;
046100111129           if  wCountSel = *zero;
046200111129             wSQL += ' where';
046300111129           else;
046400111129             wSQL += ' and';
046500111129           endif;
046600111129           wSQL += ' ALAare = ' + %trimr( %char(ParARE) );
046700111129           wCountSel += 1;
046800111129         endif;
046900111129
047000111129         // -?Selezione per filiale?
047100111129         if  ParFIL > *zero;
047200111129           if  wCountSel = *zero;
047300111129             wSQL += ' where';
047400111129           else;
047500111129             wSQL += ' and';
047600111129           endif;
047700111129           wSQL += ' ALAlna = ' + %trimr( %char(ParFIL) );
047800111129           wCountSel += 1;
047900111129         endif;
048000111129
048100111129         // -?Ordinamento?
048200111129         if  *in10;
048300111129           wSQL += ' order by ALAdiv, ALAare, ALAlna, ALAnaz, ALAprv, +
048400111129                              ALAcap, ALAloc, ALAznc, ALAspv';
048500111129         else;
048600111129           wSQL += ' order by ALAdiv, ALAare, ALAlna, ALAznc, ALAnaz, +
048700111129                              ALAprv, ALAcap, ALAloc, ALAspv';
048800111129         endif;
048900111129
049000111129         // -?Dichiarazione cursore?
049100111129         exec sql  prepare S1  from :wSql;
049200111129         exec sql  declare C1  cursor for S1;
049300111129
049400111129         // -?Apertura del cursore?
049500111129         exec sql  open C1;
049600111129
049700111129         if  SQLcode < *zero;
049800111129           FinL00 = *on;
049900111129           exsr  sr_PrintErr;
050000111129           leavesr;
050100111129         endif;
050200111129
050300111129         // -?1ª Lettura del cursore?
050400111129         exsr  LetFil;
050500111129
050600111129      /end-free
050700001106     C*
050800001106     C                   ENDSR
050900001106     C*------------------------------------------------------------------------*
051000001106     C* LETFIL - LETTURA PROSSIMO RECORD
051100001106     C*------------------------------------------------------------------------*
051200001106     C     LETFIL        BEGSR
051300001106     C*
051400001106     C* LEGGO FINO A:
051500001106     C*     - FINE FILE O SUPERATO LIMITE FINALE
051600001106     C*     - TROVATO RECORD VALIDO
051700001109     C                   movel     'N'           WRECOK
051800001106     C*
051900001106     C* Letture successive alla prima
052000111129     C*//   10              read      tiala01l                               99
052100111129     C*//  N10              read      tiala02l                               99
052200111129      /free
052300111129         exsr  sr_ReadCursor;
052400111129      /end-free
052500001106     C*
052600001106     C     *in99         ifeq      '1'                                          *FINE FILE
052700001106     C                   movel     '1'           finl00
052800001106     C                   else
052900001106     C                   exsr      chkrec                                       *CONTR.VALIDITA REC.
053000001106     C                   endif
053100001106     C*
053200001106     C                   ENDSR
053300111129
053400111129      /free
053500111129
053600111129       //--------------------------------------------------------------
053700111129       //?Lettura cursore C1.                                          ?
053800111129       //--------------------------------------------------------------
053900111129       BEGSR  sr_ReadCursor;
054000111129
054100111129         // -?Lettura cursore?
054200111129         exec sql  fetch next  from C1  into :ds_TIALA;
054300111129
054400111129         *in99 = *off;
054500111129         select;
054600111129           // -?Fine flusso?
054700111129           when  SQLcode = 100;
054800111129             *in99  = *on;
054900111129             FinL00 = *on;
055000111129           // -?Errore?
055100111129           when  SQLcode < *zero;
055200111129             *in99  = *on;
055300111129             FinL00 = *on;
055400111129             exsr  sr_PrintErr;
055500111129         endsl;
055600111129
055700111129         // -?Chiusura del cursore?
055800111129         if  *in99;
055900111129           exec sql  close C1;
056000111129         endif;
056100111129
056200111129       ENDSR;
056300111129
056400111129      /end-free
056500001106     C*------------------------------------------------------------------------*
056600001114     C* CHKREC - Routine di controllo validità record in stampa
056700001106     C*------------------------------------------------------------------------*
056800060904     C     CHKREC        BEGSR
056900001106     C*
057000060904     C                   movel     'S'           WRECOK            1
057100060904     C*--------------------
057200060904     C* CONTROLLO DISTRETTO
057300060904     C*--------------------
057400060904     C                   if        WRECOK = 'S'
057500060904     C                   if        PARDIS <> *blanks
057600060904     C                   if        aladiv <> PARDIS
057700060904     C                   movel     'N'           WRECOK
057800060904     C                   endif
057900060904     C                   if        aladiv  > PARDIS
058000060904     C                   movel     '1'           FINL00
058100060904     C                   endif
058200060904     C                   endif
058300060904     C                   endif
058400060904     C*--------------------
058500060904     C* CONTROLLO AREA
058600060904     C*--------------------
058700060904     C                   if        WRECOK = 'S'
058800060904     C                   if        PARARE <> *zeros
058900060904     C                   if        alaare <> PARARE
059000060904     C                   movel     'N'           WRECOK
059100060904     C                   endif
059200060904     C                   if        alaare  > PARARE
059300060904     C                   movel     '1'           FINL00
059400060904     C                   endif
059500060904     C                   endif
059600060904     C                   endif
059700060904     C*--------------------
059800070410     C* CONTROLLO FIL.
059900060904     C*--------------------
060000060904     C                   if        WRECOK = 'S'
060100060904     C                   if        PARFIL <> *zeros
060200060904     C                   if        alalna <> PARFIL
060300060904     C                   movel     'N'           WRECOK
060400060904     C                   endif
060500060904     C                   if        alalna  > PARFIL
060600060904     C                   movel     '1'           FINL00
060700060904     C                   endif
060800060904     C                   endif
060900060904     C                   endif
061000001106     C*--------------------
061100060904     C* CONTROLLO CAP "DA"/"A"
061200001106     C*--------------------
061300001106     C                   if        WRECOK = 'S'
061400001109     C                   if        PARCAD > *blanks
061500060901     C                   if        %subst(alacap:1:2) >= PARCAD
061600001106     C                   else
061700001106     C                   movel     'N'           WRECOK
061800001106     C                   endif
061900001106     C                   endif
062000001109     C                   if        PARCAA > *blanks
062100060901     C                   if        %subst(alacap:1:2) <= PARCAA
062200001109     C                   else
062300001109     C                   movel     'N'           WRECOK
062400001109     C                   endif
062500001109     C                   endif
062600001106     C                   endif
062700001106     C*
062800001106     C                   ENDSR
062900001106     C*------------------------------------------------------------------------*
063000001106     C* NEWDIV - OPERAZIONI PER NUOVA DIVISIONE
063100001106     C*------------------------------------------------------------------------*
063200001106     C     NEWDIV        BEGSR
063300061027     C*
063400061027     C                   SETOFF                                       20
063500001106     C*
063600001106     C* MEMORIZZA LA NUOVA DIVISIONE IN UN DEPOSITO
063700060901     C                   MOVEL     aladiv        depdiv
063800001106     C*
063900001106     C* DECODIFICA LA DIVISIONE
064000001106     C                   EXSR      DECDIV
064100001110     C*
064200001110     C* DESCRIZIONE IN TESTATA
064300001110     C                   EVAL      STPDIV='DIV  '+depdiv+' '+desdiv
064400001106     C*
064500001106     C* AZZERA I TOTALI DELLA DIVISIONE
064600001106     C                   Z-ADD     *ZEROS        TDFP1            11 1
064700001106     C                   Z-ADD     *ZEROS        TDFP2            11 1
064800001106     C                   Z-ADD     *ZEROS        TDFP3            11 1
064900001106     C                   Z-ADD     *ZEROS        TDFP4            11 1
065000060904     C                   Z-ADD     *ZEROS        TDFV1            13 3
065100060904     C                   Z-ADD     *ZEROS        TDFV2            13 3
065200060904     C                   Z-ADD     *ZEROS        TDFV3            13 3
065300060904     C                   Z-ADD     *ZEROS        TDFV4            13 3
065400001106     C                   Z-ADD     *ZEROS        TDFC1            11 0
065500001106     C                   Z-ADD     *ZEROS        TDFC2            11 0
065600001106     C                   Z-ADD     *ZEROS        TDFC3            11 0
065700001106     C                   Z-ADD     *ZEROS        TDFC4            11 0
065800001106     C                   Z-ADD     *ZEROS        TDFS1            11 0
065900001106     C                   Z-ADD     *ZEROS        TDFS2            11 0
066000001106     C                   Z-ADD     *ZEROS        TDFS3            11 0
066100001106     C                   Z-ADD     *ZEROS        TDFS4            11 0
066200001109     C                   Z-ADD     *ZEROS        TDTOS            11 0
066300001109     C                   Z-ADD     *ZEROS        TDTOC            11 0
066400001109     C                   Z-ADD     *ZEROS        TDTOP            11 1
066500001109     C                   Z-ADD     *ZEROS        TDTOV            13 3
066600001106     C*
066700001106     C                   ENDSR
066800001106     C*------------------------------------------------------------------------*
066900001106     C* NEWARE - OPERAZIONI PER NUOVA AREA
067000001106     C*------------------------------------------------------------------------*
067100001106     C     NEWARE        BEGSR
067200061027     C*
067300061027     C                   SETOFF                                       20
067400001106     C*
067500001106     C* MEMORIZZA LA NUOVA AREA IN UN DEPOSITO
067600060901     C                   Z-ADD     alaare        depare
067700001106     C*
067800001106     C* DECODIFICA L'AREA
067900001106     C                   EXSR      DECARE
068000001110     C*
068100001110     C* DESCRIZIONE IN TESTATA
068200001110     C                   EVAL      STPARE='AREA '+%editc(depare:'Z')+
068300001110     C                                    '  '+desare
068400001106     C*
068500001106     C* AZZERA I TOTALI DELL'AREA
068600001106     C                   Z-ADD     *ZEROS        TAFP1            11 1
068700001106     C                   Z-ADD     *ZEROS        TAFP2            11 1
068800001106     C                   Z-ADD     *ZEROS        TAFP3            11 1
068900001106     C                   Z-ADD     *ZEROS        TAFP4            11 1
069000001106     C                   Z-ADD     *ZEROS        TAFV1            13 3
069100001106     C                   Z-ADD     *ZEROS        TAFV2            13 3
069200001106     C                   Z-ADD     *ZEROS        TAFV3            13 3
069300001106     C                   Z-ADD     *ZEROS        TAFV4            13 3
069400001106     C                   Z-ADD     *ZEROS        TAFC1            11 0
069500001106     C                   Z-ADD     *ZEROS        TAFC2            11 0
069600001106     C                   Z-ADD     *ZEROS        TAFC3            11 0
069700001106     C                   Z-ADD     *ZEROS        TAFC4            11 0
069800001106     C                   Z-ADD     *ZEROS        TAFS1            11 0
069900001106     C                   Z-ADD     *ZEROS        TAFS2            11 0
070000001106     C                   Z-ADD     *ZEROS        TAFS3            11 0
070100001106     C                   Z-ADD     *ZEROS        TAFS4            11 0
070200001109     C                   Z-ADD     *ZEROS        TATOS            11 0
070300001109     C                   Z-ADD     *ZEROS        TATOC            11 0
070400001109     C                   Z-ADD     *ZEROS        TATOP            11 1
070500001109     C                   Z-ADD     *ZEROS        TATOV            13 3
070600001106     C*
070700001106     C                   ENDSR
070800001106     C*------------------------------------------------------------------------*
070900001106     C* NEWFIL - OPERAZIONI PER NUOVA FILIALE
071000001106     C*------------------------------------------------------------------------*
071100001106     C     NEWFIL        BEGSR
071200061027     C*
071300061027     C                   SETOFF                                       20
071400001106     C*
071500001106     C* MEMORIZZA LA NUOVA FILIALE IN UN DEPOSITO
071600060901     C                   Z-ADD     alalna        depfil
071700001106     C*
071800001106     C* DECODIFICA LA FILIALE
071900001106     C                   EXSR      DECFIL
072000070410     C*
072100070410     C* DESCRIZIONE IN TESTATA
072200070410     C                   EVAL      STPTES='FIL. '+%editc(depfil:'Z')+
072300070410     C                                    '  '+desfil
072400001106     C*
072500001106     C* AZZERA I TOTALI DELLA FILIALE
072600001106     C                   Z-ADD     *ZEROS        TFFP1            11 1
072700001106     C                   Z-ADD     *ZEROS        TFFP2            11 1
072800001106     C                   Z-ADD     *ZEROS        TFFP3            11 1
072900001106     C                   Z-ADD     *ZEROS        TFFP4            11 1
073000001106     C                   Z-ADD     *ZEROS        TFFV1            13 3
073100001106     C                   Z-ADD     *ZEROS        TFFV2            13 3
073200001106     C                   Z-ADD     *ZEROS        TFFV3            13 3
073300001106     C                   Z-ADD     *ZEROS        TFFV4            13 3
073400001106     C                   Z-ADD     *ZEROS        TFFC1            11 0
073500001106     C                   Z-ADD     *ZEROS        TFFC2            11 0
073600001106     C                   Z-ADD     *ZEROS        TFFC3            11 0
073700001106     C                   Z-ADD     *ZEROS        TFFC4            11 0
073800001106     C                   Z-ADD     *ZEROS        TFFS1            11 0
073900001106     C                   Z-ADD     *ZEROS        TFFS2            11 0
074000001106     C                   Z-ADD     *ZEROS        TFFS3            11 0
074100001106     C                   Z-ADD     *ZEROS        TFFS4            11 0
074200001109     C                   Z-ADD     *ZEROS        TFTOS            11 0
074300001109     C                   Z-ADD     *ZEROS        TFTOC            11 0
074400001109     C                   Z-ADD     *ZEROS        TFTOP            11 1
074500001109     C                   Z-ADD     *ZEROS        TFTOV            13 3
074600001106     C*
074700001106     C                   ENDSR
074800001113     C*------------------------------------------------------------------------*
074900001113     C* NEWCAP - OPERAZIONI PER NUOVO PRV/CAP
075000001113     C*------------------------------------------------------------------------*
075100001113     C     NEWCAP        BEGSR
075200061027     C*
075300061027     C                   SETOFF                                       20
075400001113     C*
075500001113     C* MEMORIZZA NUOVO DETTAGLIO
075600060901     C                   movel     alanaz        depnaz
075700060901     C                   movel     alaprv        depprv
075800060901     C                   movel     alacap        depcap
075900001113     C*
076000001113     C* AZZERA I TOTALI DEL CAP
076100001113     C                   Z-ADD     *ZEROS        TCFP1            11 1
076200001113     C                   Z-ADD     *ZEROS        TCFP2            11 1
076300001113     C                   Z-ADD     *ZEROS        TCFP3            11 1
076400001113     C                   Z-ADD     *ZEROS        TCFP4            11 1
076500001113     C                   Z-ADD     *ZEROS        TCFV1            13 3
076600001113     C                   Z-ADD     *ZEROS        TCFV2            13 3
076700001113     C                   Z-ADD     *ZEROS        TCFV3            13 3
076800001113     C                   Z-ADD     *ZEROS        TCFV4            13 3
076900001113     C                   Z-ADD     *ZEROS        TCFC1            11 0
077000001113     C                   Z-ADD     *ZEROS        TCFC2            11 0
077100001113     C                   Z-ADD     *ZEROS        TCFC3            11 0
077200001113     C                   Z-ADD     *ZEROS        TCFC4            11 0
077300001113     C                   Z-ADD     *ZEROS        TCFS1            11 0
077400001113     C                   Z-ADD     *ZEROS        TCFS2            11 0
077500001113     C                   Z-ADD     *ZEROS        TCFS3            11 0
077600001113     C                   Z-ADD     *ZEROS        TCFS4            11 0
077700001113     C                   Z-ADD     *ZEROS        TCTOS            11 0
077800001113     C                   Z-ADD     *ZEROS        TCTOC            11 0
077900001113     C                   Z-ADD     *ZEROS        TCTOP            11 1
078000001113     C                   Z-ADD     *ZEROS        TCTOV            13 3
078100001113     C*
078200001113     C                   ENDSR
078300061027     C*------------------------------------------------------------------------*
078400061027     C* NEWZNC - OPERAZIONI PER NUOVA ZONA CONSEGNA
078500061027     C*------------------------------------------------------------------------*
078600061027     C     NEWZNC        BEGSR
078700061027     C*
078800061027     C                   SETOFF                                       20
078900061027     C*
079000061027     C* MEMORIZZA NUOVO DETTAGLIO
079100061027     C                   movel     alaznc        depznc
079200061027     C*
079300061027     C* AZZERA I TOTALI DELLA ZONA CONSEGNA
079400061027     C                   Z-ADD     *ZEROS        TZFP1            11 1
079500061027     C                   Z-ADD     *ZEROS        TZFP2            11 1
079600061027     C                   Z-ADD     *ZEROS        TZFP3            11 1
079700061027     C                   Z-ADD     *ZEROS        TZFP4            11 1
079800061027     C                   Z-ADD     *ZEROS        TZFV1            13 3
079900061027     C                   Z-ADD     *ZEROS        TZFV2            13 3
080000061027     C                   Z-ADD     *ZEROS        TZFV3            13 3
080100061027     C                   Z-ADD     *ZEROS        TZFV4            13 3
080200061027     C                   Z-ADD     *ZEROS        TZFC1            11 0
080300061027     C                   Z-ADD     *ZEROS        TZFC2            11 0
080400061027     C                   Z-ADD     *ZEROS        TZFC3            11 0
080500061027     C                   Z-ADD     *ZEROS        TZFC4            11 0
080600061027     C                   Z-ADD     *ZEROS        TZFS1            11 0
080700061027     C                   Z-ADD     *ZEROS        TZFS2            11 0
080800061027     C                   Z-ADD     *ZEROS        TZFS3            11 0
080900061027     C                   Z-ADD     *ZEROS        TZFS4            11 0
081000061027     C                   Z-ADD     *ZEROS        TZTOS            11 0
081100061027     C                   Z-ADD     *ZEROS        TZTOC            11 0
081200061027     C                   Z-ADD     *ZEROS        TZTOP            11 1
081300061027     C                   Z-ADD     *ZEROS        TZTOV            13 3
081400061027     C*
081500061027     C                   ENDSR
081600001106     C*------------------------------------------------------------------------*
081700061027     C* NEWDET_C - OPERAZIONI PER NUOVO PRV/CAP/LOC - ORD X CAP
081800001106     C*------------------------------------------------------------------------*
081900061027     C     NEWDET_C      BEGSR
082000001106     C*
082100001106     C* MEMORIZZA NUOVO DETTAGLIO
082200060901     C                   movel     alaloc        deploc
082300061030     C*
082400061030     C* REPERISCO SE LOCALITA' CORRENTE OBSOLETA
082500061030     C                   eval      cplVER = o96ver
082600061030     C                   eval      cplNAR = *blanks
082700061030     C                   eval      cplCAP = alacap
082800061030     C                   eval      cplLOC = alaloc
082900061030     C     KEYcpl01_c    chain     azcpl01l
083000061030     C                   if        %found(azcpl01l)
083100061030     C                   eval      depfi1 = cplFI1
083200061030     C                   else
083300061030     C                   eval      depfi1 = *blanks
083400061030     C                   endif
083500001106     C*
083600001106     C* AZZERA I TOTALI DEL DETTAGLIO
083700001106     C                   Z-ADD     *ZEROS        TLFP1            11 1
083800001106     C                   Z-ADD     *ZEROS        TLFP2            11 1
083900001106     C                   Z-ADD     *ZEROS        TLFP3            11 1
084000001106     C                   Z-ADD     *ZEROS        TLFP4            11 1
084100001106     C                   Z-ADD     *ZEROS        TLFV1            13 3
084200001106     C                   Z-ADD     *ZEROS        TLFV2            13 3
084300001106     C                   Z-ADD     *ZEROS        TLFV3            13 3
084400001106     C                   Z-ADD     *ZEROS        TLFV4            13 3
084500001106     C                   Z-ADD     *ZEROS        TLFC1            11 0
084600001106     C                   Z-ADD     *ZEROS        TLFC2            11 0
084700001106     C                   Z-ADD     *ZEROS        TLFC3            11 0
084800001106     C                   Z-ADD     *ZEROS        TLFC4            11 0
084900001106     C                   Z-ADD     *ZEROS        TLFS1            11 0
085000001106     C                   Z-ADD     *ZEROS        TLFS2            11 0
085100001106     C                   Z-ADD     *ZEROS        TLFS3            11 0
085200001106     C                   Z-ADD     *ZEROS        TLFS4            11 0
085300001109     C                   Z-ADD     *ZEROS        TLTOS            11 0
085400001109     C                   Z-ADD     *ZEROS        TLTOC            11 0
085500001109     C                   Z-ADD     *ZEROS        TLTOP            11 1
085600001109     C                   Z-ADD     *ZEROS        TLTOV            13 3
085700001106     C*
085800001106     C                   ENDSR
085900061027     C*------------------------------------------------------------------------*
086000061027     C* NEWDET_Z - OPERAZIONI PER NUOVO PRV/CAP/LOC - ORD X ZNC
086100061027     C*------------------------------------------------------------------------*
086200061027     C     NEWDET_Z      BEGSR
086300061027     C*
086400061027     C* MEMORIZZA NUOVO DETTAGLIO
086500061027     C                   movel     alaprv        depprv
086600061027     C                   movel     alacap        depcap
086700061027     C                   movel     alaloc        deploc
086800061030     C*
086900061030     C* REPERISCO SE LOCALITA' CORRENTE OBSOLETA
087000061030     C                   eval      cplVER = o96ver
087100061030     C                   eval      cplNAR = *blanks
087200061030     C                   eval      cplCAP = alacap
087300061030     C                   eval      cplLOC = alaloc
087400061030     C     KEYcpl01_c    chain     azcpl01l
087500061030     C                   if        %found(azcpl01l)
087600061030     C                   eval      depfi1 = cplFI1
087700061030     C                   else
087800061030     C                   eval      depfi1 = *blanks
087900061030     C                   endif
088000061027     C*
088100061027     C* AZZERA I TOTALI DEL DETTAGLIO
088200061027     C                   Z-ADD     *ZEROS        TLFP1            11 1
088300061027     C                   Z-ADD     *ZEROS        TLFP2            11 1
088400061027     C                   Z-ADD     *ZEROS        TLFP3            11 1
088500061027     C                   Z-ADD     *ZEROS        TLFP4            11 1
088600061027     C                   Z-ADD     *ZEROS        TLFV1            13 3
088700061027     C                   Z-ADD     *ZEROS        TLFV2            13 3
088800061027     C                   Z-ADD     *ZEROS        TLFV3            13 3
088900061027     C                   Z-ADD     *ZEROS        TLFV4            13 3
089000061027     C                   Z-ADD     *ZEROS        TLFC1            11 0
089100061027     C                   Z-ADD     *ZEROS        TLFC2            11 0
089200061027     C                   Z-ADD     *ZEROS        TLFC3            11 0
089300061027     C                   Z-ADD     *ZEROS        TLFC4            11 0
089400061027     C                   Z-ADD     *ZEROS        TLFS1            11 0
089500061027     C                   Z-ADD     *ZEROS        TLFS2            11 0
089600061027     C                   Z-ADD     *ZEROS        TLFS3            11 0
089700061027     C                   Z-ADD     *ZEROS        TLFS4            11 0
089800061027     C                   Z-ADD     *ZEROS        TLTOS            11 0
089900061027     C                   Z-ADD     *ZEROS        TLTOC            11 0
090000061027     C                   Z-ADD     *ZEROS        TLTOP            11 1
090100061027     C                   Z-ADD     *ZEROS        TLTOV            13 3
090200061027     C*
090300061027     C                   ENDSR
090400001106     C*------------------------------------------------------------------------*
090500001106     C* MEMSPE - MEMORIZZA I DATI DI DETTAGLIO
090600001106     C*------------------------------------------------------------------------*
090700060904     C     MEMSPE        BEGSR
090800060901     C*
090900060904     C* TESTO CORRISPONDENZA FASCE DI PESO / VOLUME RISPETTO AI PARAMETRI RICHIESTI
091000060904     C                   SETOFF                                       303132
091100060904     C                   SETOFF                                       33
091200060904     C*
091300060904     C                   MOVE(P)   alaSPV        depspv
091400060904     C*
091500060904     C* => 1° FASCIA DI PESO IN STAMPA
091600060904     C                   IF        PARKG1DA > *zeros OR PARKG1A > *zeros
091700060904     C                   IF        depSPV  > PARKG1DA AND
091800060904     C                             depSPV <= PARKG1A
091900060904     C                   SETON                                        30
092000060904     C                   ENDIF
092100060904     C                   ENDIF
092200060904     C*
092300060904     C* => 2° FASCIA DI PESO IN STAMPA
092400060904     C                   IF        PARKG2DA > *zeros OR PARKG2A > *zeros
092500060904     C                   IF        depSPV  > PARKG2DA AND
092600060904     C                             depSPV <= PARKG2A
092700060904     C                   SETON                                        31
092800060904     C                   ENDIF
092900060904     C                   ENDIF
093000060904     C*
093100060904     C* => 3° FASCIA DI PESO IN STAMPA
093200060904     C                   IF        PARKG3DA > *zeros OR PARKG3A > *zeros
093300060904     C                   IF        depSPV  > PARKG3DA AND
093400060904     C                             depSPV <= PARKG3A
093500060904     C                   SETON                                        32
093600060904     C                   ENDIF
093700060904     C                   ENDIF
093800060904     C*
093900060904     C* => 4° FASCIA DI PESO IN STAMPA
094000060904     C                   IF        PARKG4DA > *zeros OR PARKG4A > *zeros
094100060904     C                   IF        depSPV  > PARKG4DA AND
094200060904     C                             depSPV <= PARKG4A
094300060904     C                   SETON                                        33
094400060904     C                   ENDIF
094500060904     C                   ENDIF
094600001106     C*
094700001109     C* TOTALE PRV/CAP/LOC (DETTAGLIO)
094800060904     C   30              ADD       alatop        TLFP1
094900060904     C   31              ADD       alatop        TLFP2
095000060904     C   32              ADD       alatop        TLFP3
095100060904     C   33              ADD       alatop        TLFP4
095200060904     C   30              ADD       alatov        TLFV1
095300060904     C   31              ADD       alatov        TLFV2
095400060904     C   32              ADD       alatov        TLFV3
095500060904     C   33              ADD       alatov        TLFV4
095600060904     C   30              ADD       alatoc        TLFC1
095700060904     C   31              ADD       alatoc        TLFC2
095800060904     C   32              ADD       alatoc        TLFC3
095900060904     C   33              ADD       alatoc        TLFC4
096000060904     C   30              ADD       alatos        TLFS1
096100060904     C   31              ADD       alatos        TLFS2
096200060904     C   32              ADD       alatos        TLFS3
096300060904     C   33              ADD       alatos        TLFS4
096400060904     C                   IF        *in30=*on OR *in31=*on OR
096500060904     C                             *in32=*on OR *in33=*on
096600060901     C                   ADD       alatos        TLTOS
096700060901     C                   ADD       alatoc        TLTOC
096800060901     C                   ADD       alatop        TLTOP
096900060901     C                   ADD       alatov        TLTOV
097000060904     C                   ENDIF
097100061027     C* TOTALE ZNC
097200061027     C   30              ADD       alatop        TZFP1
097300061027     C   31              ADD       alatop        TZFP2
097400061027     C   32              ADD       alatop        TZFP3
097500061027     C   33              ADD       alatop        TZFP4
097600061027     C   30              ADD       alatov        TZFV1
097700061027     C   31              ADD       alatov        TZFV2
097800061027     C   32              ADD       alatov        TZFV3
097900061027     C   33              ADD       alatov        TZFV4
098000061027     C   30              ADD       alatoc        TZFC1
098100061027     C   31              ADD       alatoc        TZFC2
098200061027     C   32              ADD       alatoc        TZFC3
098300061027     C   33              ADD       alatoc        TZFC4
098400061027     C   30              ADD       alatos        TZFS1
098500061027     C   31              ADD       alatos        TZFS2
098600061027     C   32              ADD       alatos        TZFS3
098700061027     C   33              ADD       alatos        TZFS4
098800061027     C                   IF        *in30=*on OR *in31=*on OR
098900061027     C                             *in32=*on OR *in33=*on
099000061027     C                   ADD       alatos        TZTOS
099100061027     C                   ADD       alatoc        TZTOC
099200061027     C                   ADD       alatop        TZTOP
099300061027     C                   ADD       alatov        TZTOV
099400061027     C                   ENDIF
099500001113     C* TOTALE PRV/CAP
099600060904     C   30              ADD       alatop        TCFP1
099700060904     C   31              ADD       alatop        TCFP2
099800060904     C   32              ADD       alatop        TCFP3
099900060904     C   33              ADD       alatop        TCFP4
100000060904     C   30              ADD       alatov        TCFV1
100100060904     C   31              ADD       alatov        TCFV2
100200060904     C   32              ADD       alatov        TCFV3
100300060904     C   33              ADD       alatov        TCFV4
100400060904     C   30              ADD       alatoc        TCFC1
100500060904     C   31              ADD       alatoc        TCFC2
100600060904     C   32              ADD       alatoc        TCFC3
100700060904     C   33              ADD       alatoc        TCFC4
100800060904     C   30              ADD       alatos        TCFS1
100900060904     C   31              ADD       alatos        TCFS2
101000060904     C   32              ADD       alatos        TCFS3
101100060904     C   33              ADD       alatos        TCFS4
101200060904     C                   IF        *in30=*on OR *in31=*on OR
101300060904     C                             *in32=*on OR *in33=*on
101400060904     C                   ADD       alatos        TCTOS
101500060904     C                   ADD       alatoc        TCTOC
101600060904     C                   ADD       alatop        TCTOP
101700060904     C                   ADD       alatov        TCTOV
101800060904     C                   ENDIF
101900001106     C* TOTALE FILIALE
102000060904     C   30              ADD       alatop        TFFP1
102100060904     C   31              ADD       alatop        TFFP2
102200060904     C   32              ADD       alatop        TFFP3
102300060904     C   33              ADD       alatop        TFFP4
102400060904     C   30              ADD       alatov        TFFV1
102500060904     C   31              ADD       alatov        TFFV2
102600060904     C   32              ADD       alatov        TFFV3
102700060904     C   33              ADD       alatov        TFFV4
102800060904     C   30              ADD       alatoc        TFFC1
102900060904     C   31              ADD       alatoc        TFFC2
103000060904     C   32              ADD       alatoc        TFFC3
103100060904     C   33              ADD       alatoc        TFFC4
103200060904     C   30              ADD       alatos        TFFS1
103300060904     C   31              ADD       alatos        TFFS2
103400060904     C   32              ADD       alatos        TFFS3
103500060904     C   33              ADD       alatos        TFFS4
103600060904     C                   IF        *in30=*on OR *in31=*on OR
103700060904     C                             *in32=*on OR *in33=*on
103800060904     C                   ADD       alatos        TFTOS
103900060904     C                   ADD       alatoc        TFTOC
104000060904     C                   ADD       alatop        TFTOP
104100060904     C                   ADD       alatov        TFTOV
104200060904     C                   ENDIF
104300001106     C* TOTALE AREA
104400060904     C   30              ADD       alatop        TAFP1
104500060904     C   31              ADD       alatop        TAFP2
104600060904     C   32              ADD       alatop        TAFP3
104700060904     C   33              ADD       alatop        TAFP4
104800060904     C   30              ADD       alatov        TAFV1
104900060904     C   31              ADD       alatov        TAFV2
105000060904     C   32              ADD       alatov        TAFV3
105100060904     C   33              ADD       alatov        TAFV4
105200060904     C   30              ADD       alatoc        TAFC1
105300060904     C   31              ADD       alatoc        TAFC2
105400060904     C   32              ADD       alatoc        TAFC3
105500060904     C   33              ADD       alatoc        TAFC4
105600060904     C   30              ADD       alatos        TAFS1
105700060904     C   31              ADD       alatos        TAFS2
105800060904     C   32              ADD       alatos        TAFS3
105900060904     C   33              ADD       alatos        TAFS4
106000060904     C                   IF        *in30=*on OR *in31=*on OR
106100060904     C                             *in32=*on OR *in33=*on
106200060904     C                   ADD       alatos        TATOS
106300060904     C                   ADD       alatoc        TATOC
106400060904     C                   ADD       alatop        TATOP
106500060904     C                   ADD       alatov        TATOV
106600060904     C                   ENDIF
106700001106     C* TOTALE DIVISIONE
106800060904     C   30              ADD       alatop        TDFP1
106900060904     C   31              ADD       alatop        TDFP2
107000060904     C   32              ADD       alatop        TDFP3
107100060904     C   33              ADD       alatop        TDFP4
107200060904     C   30              ADD       alatov        TDFV1
107300060904     C   31              ADD       alatov        TDFV2
107400060904     C   32              ADD       alatov        TDFV3
107500060904     C   33              ADD       alatov        TDFV4
107600060904     C   30              ADD       alatoc        TDFC1
107700060904     C   31              ADD       alatoc        TDFC2
107800060904     C   32              ADD       alatoc        TDFC3
107900060904     C   33              ADD       alatoc        TDFC4
108000060904     C   30              ADD       alatos        TDFS1
108100060904     C   31              ADD       alatos        TDFS2
108200060904     C   32              ADD       alatos        TDFS3
108300060904     C   33              ADD       alatos        TDFS4
108400060904     C                   IF        *in30=*on OR *in31=*on OR
108500060904     C                             *in32=*on OR *in33=*on
108600060904     C                   ADD       alatos        TDTOS
108700060904     C                   ADD       alatoc        TDTOC
108800060904     C                   ADD       alatop        TDTOP
108900060904     C                   ADD       alatov        TDTOV
109000060904     C                   ENDIF
109100001114     C* TOTALE GENERALE
109200060904     C   30              ADD       alatop        TGFP1            11 1
109300060904     C   31              ADD       alatop        TGFP2            11 1
109400060904     C   32              ADD       alatop        TGFP3            11 1
109500060904     C   33              ADD       alatop        TGFP4            11 1
109600060904     C   30              ADD       alatov        TGFV1            13 3
109700060904     C   31              ADD       alatov        TGFV2            13 3
109800060904     C   32              ADD       alatov        TGFV3            13 3
109900060904     C   33              ADD       alatov        TGFV4            13 3
110000060904     C   30              ADD       alatoc        TGFC1            11 0
110100060904     C   31              ADD       alatoc        TGFC2            11 0
110200060904     C   32              ADD       alatoc        TGFC3            11 0
110300060904     C   33              ADD       alatoc        TGFC4            11 0
110400060904     C   30              ADD       alatos        TGFS1            11 0
110500060904     C   31              ADD       alatos        TGFS2            11 0
110600060904     C   32              ADD       alatos        TGFS3            11 0
110700060904     C   33              ADD       alatos        TGFS4            11 0
110800060904     C                   IF        *in30=*on OR *in31=*on OR
110900060904     C                             *in32=*on OR *in33=*on
111000060901     C                   ADD       alatos        TGTOS            11 0
111100060901     C                   ADD       alatoc        TGTOC            11 0
111200060901     C                   ADD       alatop        TGTOP            11 1
111300060901     C                   ADD       alatov        TGTOV            13 3
111400060904     C                   ENDIF
111500001106     C*
111600001106     C                   ENDSR
111700001106     C*------------------------------------------------------------------------*
111800001106     C* DECDIV - DECODIFICA DIVISIONE
111900001106     C*------------------------------------------------------------------------*
112000001106     C     DECDIV        BEGSR
112100001106     C*
112200060904     C                   MOVEL     1             tblKUT
112300060904     C                   MOVE      '17'          tblCOD
112400060904     C                   MOVEL(P)  depdiv        tblKEY
112500060904     C     KEYtab_c      CHAIN     TABEL                              97
112600001106     C     *IN97         IFEQ      '1'
112700001106     C     TBLFLG        OREQ      '*'
112800001106     C                   MOVEL     *ALL'*'       DESDIV           20
112900001106     C                   ELSE
113000001106     C                   MOVEL     TBLUNI        DESDIV
113100001106     C                   ENDIF
113200001106     C*
113300060904     C                   ENDSR
113400001106     C*------------------------------------------------------------------------*
113500001106     C* DECARE - DECODIFICA AREA
113600001106     C*------------------------------------------------------------------------*
113700001106     C     DECARE        BEGSR
113800001106     C*
113900060904     C                   MOVEL     1             tblKUT
114000060904     C                   MOVE      '05'          tblCOD
114100060904     C                   MOVEL(P)  depare        tblKEY
114200060904     C     KEYtab_c      CHAIN     TABEL                              97
114300001106     C     *IN97         IFEQ      '1'
114400001106     C     TBLFLG        OREQ      '*'
114500001106     C                   MOVEL     *ALL'*'       DESARE           20
114600001106     C                   ELSE
114700001106     C                   MOVEL     TBLUNI        DESARE
114800001106     C                   ENDIF
114900001106     C*
115000060904     C                   ENDSR
115100001106     C*------------------------------------------------------------------------*
115200001106     C* DECFIL - DECODIFICA FILIALE/TERMINAL
115300001106     C*------------------------------------------------------------------------*
115400001106     C     DECFIL        BEGSR
115500001106     C*
115600060904     C                   Z-ADD     1             I                 3 0
115700001106     C     DEPFIL        LOOKUP    FIL(I)                                 95
115800001106     C     *IN95         IFEQ      '0'
115900001106     C                   MOVEL     *ALL'*'       DESFIL           20
116000001106     C                   ELSE
116100001106     C                   MOVEL     DFIL(I)       DESFIL
116200001106     C                   ENDIF
116300001106     C*
116400060904     C                   ENDSR
116500001106     C*------------------------------------------------------------------------*
116600001109     C* STADET - STAMPA RIGA DI DETTAGLIO
116700001106     C*------------------------------------------------------------------------*
116800001106     C     STADET        BEGSR
116900001109     C*
117000070410     C                   EVAL      STPTES='FIL. '+%editc(depfil:'Z')+
117100001113     C                                    '  '+desfil
117200001113     C                   EVAL      STPDES = depprv+' '+%subst(depcap:1:5)+
117300061030     C                                      ' '+depfi1+deploc
117400061027     C  N10              MOVE      depznc        STPDES
117500001113     C* VALORIZZO ALTRI DATI IN STAMPA
117600001120     C                   EVAL(H)   STPFP1 = TLFP1 / PARGIO / 100
117700001120     C                   EVAL(H)   STPFP2 = TLFP2 / PARGIO / 100
117800001120     C                   EVAL(H)   STPFP3 = TLFP3 / PARGIO / 100
117900001120     C                   EVAL(H)   STPFP4 = TLFP4 / PARGIO / 100
118000001120     C                   EVAL(H)   STPFV1 = TLFV1 / PARGIO
118100001120     C                   EVAL(H)   STPFV2 = TLFV2 / PARGIO
118200001120     C                   EVAL(H)   STPFV3 = TLFV3 / PARGIO
118300001120     C                   EVAL(H)   STPFV4 = TLFV4 / PARGIO
118400001120     C                   EVAL(H)   STPFC1 = TLFC1 / PARGIO
118500001120     C                   EVAL(H)   STPFC2 = TLFC2 / PARGIO
118600001120     C                   EVAL(H)   STPFC3 = TLFC3 / PARGIO
118700001120     C                   EVAL(H)   STPFC4 = TLFC4 / PARGIO
118800001120     C                   EVAL(H)   STPFS1 = TLFS1 / PARGIO
118900001120     C                   EVAL(H)   STPFS2 = TLFS2 / PARGIO
119000001120     C                   EVAL(H)   STPFS3 = TLFS3 / PARGIO
119100001120     C                   EVAL(H)   STPFS4 = TLFS4 / PARGIO
119200001120     C                   EVAL(H)   STPTOS = TLTOS / PARGIO
119300001120     C                   EVAL(H)   STPTOC = TLTOC / PARGIO
119400001120     C                   EVAL(H)   STPTOP = TLTOP / PARGIO / 100
119500001120     C                   EVAL(H)   STPTOV = TLTOV / PARGIO
119600001113     C*
119700001113     C* STAMPO SOLO SE ESISTONO SPEDIZIONI
119800001113     C                   IF        STPTOS > 0
119900001109     C*
120000001109     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
120100001109     C     *IN01         IFEQ      '1'
120200060901     C                   WRITE     ST48TE                                       *TESTATA
120300001110     C                   SETOFF                                       01OF
120400001109     C                   ENDIF
120500060901     C                   WRITE     ST48DE                                       *DETTAGLIO
120600061027     C                   SETON                                        20
120700001110     C                   ENDIF
120800001106     C*
120900001106     C                   ENDSR
121000061027     C*------------------------------------------------------------------------*
121100061027     C* STAZNC - STAMPA TOTALI DELLA ZONA CONSEGNA
121200061027     C*------------------------------------------------------------------------*
121300061027     C     STAZNC        BEGSR
121400061027     C*
121500061027     C                   EVAL      STPDES='       ZNC  '+%editc(depznc:'Z')
121600061027     C* VALORIZZO ALTRI DATI IN STAMPA
121700061027     C                   EVAL(H)   STPFP1 = TZFP1 / PARGIO / 100
121800061027     C                   EVAL(H)   STPFP2 = TZFP2 / PARGIO / 100
121900061027     C                   EVAL(H)   STPFP3 = TZFP3 / PARGIO / 100
122000061027     C                   EVAL(H)   STPFP4 = TZFP4 / PARGIO / 100
122100061027     C                   EVAL(H)   STPFV1 = TZFV1 / PARGIO
122200061027     C                   EVAL(H)   STPFV2 = TZFV2 / PARGIO
122300061027     C                   EVAL(H)   STPFV3 = TZFV3 / PARGIO
122400061027     C                   EVAL(H)   STPFV4 = TZFV4 / PARGIO
122500061027     C                   EVAL(H)   STPFC1 = TZFC1 / PARGIO
122600061027     C                   EVAL(H)   STPFC2 = TZFC2 / PARGIO
122700061027     C                   EVAL(H)   STPFC3 = TZFC3 / PARGIO
122800061027     C                   EVAL(H)   STPFC4 = TZFC4 / PARGIO
122900061027     C                   EVAL(H)   STPFS1 = TZFS1 / PARGIO
123000061027     C                   EVAL(H)   STPFS2 = TZFS2 / PARGIO
123100061027     C                   EVAL(H)   STPFS3 = TZFS3 / PARGIO
123200061027     C                   EVAL(H)   STPFS4 = TZFS4 / PARGIO
123300061027     C                   EVAL(H)   STPTOS = TZTOS / PARGIO
123400061027     C                   EVAL(H)   STPTOC = TZTOC / PARGIO
123500061027     C                   EVAL(H)   STPTOP = TZTOP / PARGIO / 100
123600061027     C                   EVAL(H)   STPTOV = TZTOV / PARGIO
123700061027     C*
123800061027     C* STAMPO SOLO SE ESISTONO SPEDIZIONI
123900061027     C                   IF        STPTOS > 0
124000061027     C*
124100061027     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
124200061027     C     *IN01         IFEQ      '1'
124300061027     C                   WRITE     ST48TE                                       *TESTATA
124400061027     C                   SETOFF                                       01OF
124500061027     C                   ENDIF
124600061027     C                   WRITE     ST48DE                                       *DETTAGLIO
124700061027     C                   ENDIF
124800061027     C*
124900061027     C                   SETOFF                                       20
125000061027     C*
125100061027     C                   ENDSR
125200001113     C*------------------------------------------------------------------------*
125300001113     C* STACAP - STAMPA TOTALI DEL CAP
125400001113     C*------------------------------------------------------------------------*
125500001113     C     STACAP        BEGSR
125600001113     C*
125700001113     C                   EVAL      STPDES='       CAP  '+%subst(depcap:1:5)
125800001113     C* VALORIZZO ALTRI DATI IN STAMPA
125900001120     C                   EVAL(H)   STPFP1 = TCFP1 / PARGIO / 100
126000001120     C                   EVAL(H)   STPFP2 = TCFP2 / PARGIO / 100
126100001120     C                   EVAL(H)   STPFP3 = TCFP3 / PARGIO / 100
126200001120     C                   EVAL(H)   STPFP4 = TCFP4 / PARGIO / 100
126300001120     C                   EVAL(H)   STPFV1 = TCFV1 / PARGIO
126400001120     C                   EVAL(H)   STPFV2 = TCFV2 / PARGIO
126500001120     C                   EVAL(H)   STPFV3 = TCFV3 / PARGIO
126600001120     C                   EVAL(H)   STPFV4 = TCFV4 / PARGIO
126700001120     C                   EVAL(H)   STPFC1 = TCFC1 / PARGIO
126800001120     C                   EVAL(H)   STPFC2 = TCFC2 / PARGIO
126900001120     C                   EVAL(H)   STPFC3 = TCFC3 / PARGIO
127000001120     C                   EVAL(H)   STPFC4 = TCFC4 / PARGIO
127100001120     C                   EVAL(H)   STPFS1 = TCFS1 / PARGIO
127200001120     C                   EVAL(H)   STPFS2 = TCFS2 / PARGIO
127300001120     C                   EVAL(H)   STPFS3 = TCFS3 / PARGIO
127400001120     C                   EVAL(H)   STPFS4 = TCFS4 / PARGIO
127500001120     C                   EVAL(H)   STPTOS = TCTOS / PARGIO
127600001120     C                   EVAL(H)   STPTOC = TCTOC / PARGIO
127700001120     C                   EVAL(H)   STPTOP = TCTOP / PARGIO / 100
127800001120     C                   EVAL(H)   STPTOV = TCTOV / PARGIO
127900001113     C*
128000001113     C* STAMPO SOLO SE ESISTONO SPEDIZIONI
128100001113     C                   IF        STPTOS > 0
128200001113     C*
128300001113     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
128400001113     C     *IN01         IFEQ      '1'
128500060901     C                   WRITE     ST48TE                                       *TESTATA
128600001113     C                   SETOFF                                       01OF
128700001113     C                   ENDIF
128800060901     C                   WRITE     ST48DE                                       *DETTAGLIO
128900001113     C                   ENDIF
129000001113     C*
129100061027     C                   SETOFF                                       20
129200061027     C*
129300001113     C                   ENDSR
129400001109     C*------------------------------------------------------------------------*
129500001109     C* STAFIL - STAMPA TOTALI DI FILIALE
129600001109     C*------------------------------------------------------------------------*
129700001109     C     STAFIL        BEGSR
129800001109     C*
129900070410     C                   EVAL      STPTES='FIL.        '+%editc(depfil:'Z')+
130000001109     C                                    '  '+desfil
130100070410     C                   EVAL      STPDES='TOTALE FIL. '+%editc(depfil:'Z')+
130200001109     C                                    '  '+desfil
130300001113     C* VALORIZZO ALTRI DATI IN STAMPA
130400001120     C                   EVAL(H)   STPFP1 = TFFP1 / PARGIO / 100
130500001120     C                   EVAL(H)   STPFP2 = TFFP2 / PARGIO / 100
130600001120     C                   EVAL(H)   STPFP3 = TFFP3 / PARGIO / 100
130700001120     C                   EVAL(H)   STPFP4 = TFFP4 / PARGIO / 100
130800001120     C                   EVAL(H)   STPFV1 = TFFV1 / PARGIO
130900001120     C                   EVAL(H)   STPFV2 = TFFV2 / PARGIO
131000001120     C                   EVAL(H)   STPFV3 = TFFV3 / PARGIO
131100001120     C                   EVAL(H)   STPFV4 = TFFV4 / PARGIO
131200001120     C                   EVAL(H)   STPFC1 = TFFC1 / PARGIO
131300001120     C                   EVAL(H)   STPFC2 = TFFC2 / PARGIO
131400001120     C                   EVAL(H)   STPFC3 = TFFC3 / PARGIO
131500001120     C                   EVAL(H)   STPFC4 = TFFC4 / PARGIO
131600001120     C                   EVAL(H)   STPFS1 = TFFS1 / PARGIO
131700001120     C                   EVAL(H)   STPFS2 = TFFS2 / PARGIO
131800001120     C                   EVAL(H)   STPFS3 = TFFS3 / PARGIO
131900001120     C                   EVAL(H)   STPFS4 = TFFS4 / PARGIO
132000001120     C                   EVAL(H)   STPTOS = TFTOS / PARGIO
132100001120     C                   EVAL(H)   STPTOC = TFTOC / PARGIO
132200001120     C                   EVAL(H)   STPTOP = TFTOP / PARGIO / 100
132300001120     C                   EVAL(H)   STPTOV = TFTOV / PARGIO
132400001113     C*
132500001113     C* STAMPO SOLO SE ESISTONO SPEDIZIONI
132600001113     C                   IF        STPTOS > 0
132700001109     C*
132800001109     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
132900001110     C     *IN01         IFEQ      '1'
133000060901     C                   WRITE     ST48TE                                       *TESTATA
133100001110     C                   SETOFF                                       OF
133200001110     C                   ENDIF
133300001110     C                   SETON                                        01
133400060901     C                   WRITE     ST48RI                                       *SEPARAZIONE
133500060901     C                   WRITE     ST48DE                                       *DETTAGLIO
133600060901     C                   WRITE     ST48RI                                       *SEPARAZIONE
133700001110     C                   ENDIF
133800001109     C*
133900001109     C                   ENDSR
134000001109     C*------------------------------------------------------------------------*
134100001109     C* STAARE - STAMPA TOTALI DI AREA
134200001109     C*------------------------------------------------------------------------*
134300001109     C     STAARE        BEGSR
134400001109     C*
134500001109     C                   EVAL      STPTES='AREA        '+%editc(depare:'Z')+' '+
134600001109     C                                    desare
134700001109     C                   EVAL      STPDES='TOTALE AREA '+%editc(depare:'Z')+' '+
134800001109     C                                    desare
134900001113     C* VALORIZZO ALTRI DATI IN STAMPA
135000001120     C                   EVAL(H)   STPFP1 = TAFP1 / PARGIO / 100
135100001120     C                   EVAL(H)   STPFP2 = TAFP2 / PARGIO / 100
135200001120     C                   EVAL(H)   STPFP3 = TAFP3 / PARGIO / 100
135300001120     C                   EVAL(H)   STPFP4 = TAFP4 / PARGIO / 100
135400001120     C                   EVAL(H)   STPFV1 = TAFV1 / PARGIO
135500001120     C                   EVAL(H)   STPFV2 = TAFV2 / PARGIO
135600001120     C                   EVAL(H)   STPFV3 = TAFV3 / PARGIO
135700001120     C                   EVAL(H)   STPFV4 = TAFV4 / PARGIO
135800001120     C                   EVAL(H)   STPFC1 = TAFC1 / PARGIO
135900001120     C                   EVAL(H)   STPFC2 = TAFC2 / PARGIO
136000001120     C                   EVAL(H)   STPFC3 = TAFC3 / PARGIO
136100001120     C                   EVAL(H)   STPFC4 = TAFC4 / PARGIO
136200001120     C                   EVAL(H)   STPFS1 = TAFS1 / PARGIO
136300001120     C                   EVAL(H)   STPFS2 = TAFS2 / PARGIO
136400001120     C                   EVAL(H)   STPFS3 = TAFS3 / PARGIO
136500001120     C                   EVAL(H)   STPFS4 = TAFS4 / PARGIO
136600001120     C                   EVAL(H)   STPTOS = TATOS / PARGIO
136700001120     C                   EVAL(H)   STPTOC = TATOC / PARGIO
136800001120     C                   EVAL(H)   STPTOP = TATOP / PARGIO / 100
136900001120     C                   EVAL(H)   STPTOV = TATOV / PARGIO
137000001113     C*
137100001113     C* PROSEGUO E STAMPO SOLO SE ESISTONO SPEDIZIONI
137200001113     C                   IF        STPTOS > 0
137300001109     C*
137400001109     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
137500001110     C     *IN01         IFEQ      '1'
137600060901     C                   WRITE     ST48TE                                       *TESTATA
137700001110     C                   SETOFF                                       OF
137800001110     C                   ENDIF
137900001110     C                   SETON                                        01
138000060901     C                   WRITE     ST48RI                                       *SEPARAZIONE
138100060901     C                   WRITE     ST48DE                                       *DETTAGLIO
138200060901     C                   WRITE     ST48RI                                       *SEPARAZIONE
138300001110     C                   ENDIF
138400001109     C*
138500001109     C                   ENDSR
138600001109     C*------------------------------------------------------------------------*
138700001109     C* STADIV - STAMPA TOTALI DI DIVISIONE
138800001109     C*------------------------------------------------------------------------*
138900001109     C     STADIV        BEGSR
139000001109     C*
139100001109     C                   EVAL      STPTES='DIVISIONE        '+depdiv+' '+desdiv
139200001109     C                   EVAL      STPDES='TOTALE DIVIONE   '+depdiv+' '+desdiv
139300070410     C* ELIMINO DESCRIZIONE AREA
139400070410     C                   EVAL      STPARE=*blanks
139500001113     C* VALORIZZO ALTRI DATI IN STAMPA
139600001120     C                   EVAL(H)   STPFP1 = TDFP1 / PARGIO / 100
139700001120     C                   EVAL(H)   STPFP2 = TDFP2 / PARGIO / 100
139800001120     C                   EVAL(H)   STPFP3 = TDFP3 / PARGIO / 100
139900001120     C                   EVAL(H)   STPFP4 = TDFP4 / PARGIO / 100
140000001120     C                   EVAL(H)   STPFV1 = TDFV1 / PARGIO
140100001120     C                   EVAL(H)   STPFV2 = TDFV2 / PARGIO
140200001120     C                   EVAL(H)   STPFV3 = TDFV3 / PARGIO
140300001120     C                   EVAL(H)   STPFV4 = TDFV4 / PARGIO
140400001120     C                   EVAL(H)   STPFC1 = TDFC1 / PARGIO
140500001120     C                   EVAL(H)   STPFC2 = TDFC2 / PARGIO
140600001120     C                   EVAL(H)   STPFC3 = TDFC3 / PARGIO
140700001120     C                   EVAL(H)   STPFC4 = TDFC4 / PARGIO
140800001120     C                   EVAL(H)   STPFS1 = TDFS1 / PARGIO
140900001120     C                   EVAL(H)   STPFS2 = TDFS2 / PARGIO
141000001120     C                   EVAL(H)   STPFS3 = TDFS3 / PARGIO
141100001120     C                   EVAL(H)   STPFS4 = TDFS4 / PARGIO
141200001120     C                   EVAL(H)   STPTOS = TDTOS / PARGIO
141300001120     C                   EVAL(H)   STPTOC = TDTOC / PARGIO
141400001120     C                   EVAL(H)   STPTOP = TDTOP / PARGIO / 100
141500001120     C                   EVAL(H)   STPTOV = TDTOV / PARGIO
141600001113     C*
141700001113     C* PROSEGUO E STAMPO SOLO SE ESISTONO SPEDIZIONI
141800001113     C                   IF        STPTOS > 0
141900001109     C*
142000001109     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
142100001110     C     *IN01         IFEQ      '1'
142200060901     C                   WRITE     ST48TE                                       *TESTATA
142300001110     C                   SETOFF                                       OF
142400001110     C                   ENDIF
142500001110     C                   SETON                                        01
142600060901     C                   WRITE     ST48RI                                       *SEPARAZIONE
142700060901     C                   WRITE     ST48DE                                       *DETTAGLIO
142800060901     C                   WRITE     ST48RI                                       *SEPARAZIONE
142900001110     C                   ENDIF
143000001109     C*
143100001109     C                   ENDSR
143200001109     C*------------------------------------------------------------------------*
143300001109     C* STAGEN - STAMPA TOTALI GENERALI
143400001109     C*------------------------------------------------------------------------*
143500001109     C     STAGEN        BEGSR
143600001109     C*
143700001110     C                   EVAL      STPDIV = *BLANKS
143800001110     C                   EVAL      STPARE = *BLANKS
143900001110     C                   EVAL      STPTES = *BLANKS
144000001109     C                   EVAL      STPDES = 'TOTALE GENERALE'
144100001109     C*
144200001120     C                   EVAL(H)   STPFP1 = TGFP1 / PARGIO / 100
144300001120     C                   EVAL(H)   STPFP2 = TGFP2 / PARGIO / 100
144400001120     C                   EVAL(H)   STPFP3 = TGFP3 / PARGIO / 100
144500001120     C                   EVAL(H)   STPFP4 = TGFP4 / PARGIO / 100
144600001120     C                   EVAL(H)   STPFV1 = TGFV1 / PARGIO
144700001120     C                   EVAL(H)   STPFV2 = TGFV2 / PARGIO
144800001120     C                   EVAL(H)   STPFV3 = TGFV3 / PARGIO
144900001120     C                   EVAL(H)   STPFV4 = TGFV4 / PARGIO
145000001120     C                   EVAL(H)   STPFC1 = TGFC1 / PARGIO
145100001120     C                   EVAL(H)   STPFC2 = TGFC2 / PARGIO
145200001120     C                   EVAL(H)   STPFC3 = TGFC3 / PARGIO
145300001120     C                   EVAL(H)   STPFC4 = TGFC4 / PARGIO
145400001120     C                   EVAL(H)   STPFS1 = TGFS1 / PARGIO
145500001120     C                   EVAL(H)   STPFS2 = TGFS2 / PARGIO
145600001120     C                   EVAL(H)   STPFS3 = TGFS3 / PARGIO
145700001120     C                   EVAL(H)   STPFS4 = TGFS4 / PARGIO
145800001120     C                   EVAL(H)   STPTOS = TGTOS / PARGIO
145900001120     C                   EVAL(H)   STPTOC = TGTOC / PARGIO
146000001120     C                   EVAL(H)   STPTOP = TGTOP / PARGIO / 100
146100001120     C                   EVAL(H)   STPTOV = TGTOV / PARGIO
146200001109     C*
146300001109     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
146400001110     C     *IN01         IFEQ      '1'
146500060901     C                   WRITE     ST48TE                                       *TESTATA
146600001110     C                   SETOFF                                       OF
146700001110     C                   ENDIF
146800060901     C                   WRITE     ST48RI                                       *SEPARAZIONE
146900060901     C                   WRITE     ST48DE                                       *DETTAGLIO
147000060901     C                   WRITE     ST48RI                                       *SEPARAZIONE
147100060901     C                   WRITE     ST48FI                                       *FINE
147200001109     C*
147300060904     C                   ENDSR
147400000914     C*------------------------------------------------------------------------*
147500060904     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
147600000914     C*------------------------------------------------------------------------*
147700000914     C     CARORG        BEGSR
147800020226     C*
147900060904     C                   Z-ADD     *ZEROS        y                 3 0
148000000914     C*
148100000914     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
148200060904     C     *LOVAL        SETLL     AZORG01L                           99        *FINE-ARCHIV
148300000914     C  N99              READ      AZORG01L                               99
148400000915     C                   Z-ADD     0             y                              *INDICE SCHIERA
148500000914     C     *IN99         DOWEQ     '0'                                          --- 1 -->
148600000914     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
148700000914     C     ORGFAG        ANDNE     'V'
148800000915     C                   ADD       1             y
148900000915     C                   Z-ADD     ORGFIL        FIL(y)
149000001106     C                   MOVEL     ORGDES        DFIL(y)
149100001016     C                   ENDIF
149200000914     C                   READ      AZORG01L                               99
149300001016     C                   ENDDO
149400000914     C*
149500000914     C                   ENDSR
149600000000     C*------------------------------------------------------------------------*
149700000000     C* *INZSR - ROUTINE INIZIALE
149800000000     C*------------------------------------------------------------------------*
149900000000     C     *INZSR        BEGSR
150000000000     C*--------------------
150100000000     C* RICEVIMENTO PARAMETRI
150200000000     C*--------------------
150300000000     C     *ENTRY        PLIST
150400000000     C                   PARM                    KPJBA
150500010417     C                   MOVEL(p)  KPJBU         PARAM
150600061027     C
150700000000     C*--------------------
150800000000     C* CHIAVI DI LETTURA
150900000000     C*--------------------
151000000000     C*
151100060904     C* CHIAVE LETTURA TABEL00F - COMPLETA
151200060904     C     KEYtab_c      KLIST
151300060904     C                   KFLD                    tblKUT
151400060904     C                   KFLD                    tblCOD
151500060904     C                   KFLD                    tblKEY
151600061030     C*
151700061030     C* CHIAVE LETTURA AZCPL01L - COMPLETA
151800061030     C     KEYcpl01_c    KLIST
151900061030     C                   KFLD                    cplVER
152000061030     C                   KFLD                    cplNAR
152100061030     C                   KFLD                    cplCAP
152200061030     C                   KFLD                    cplLOC
152300000914     C*
152400111129     C*//* CHIAVE LETTURA TIALA01L - COMPLETA
152500111129     C*//     KEYala01_c    KLIST
152600111129     C*//                   KFLD                    k_alaDIV
152700111129     C*//                   KFLD                    k_alaARE
152800111129     C*//                   KFLD                    k_alaLNA
152900111129     C*//                   KFLD                    k_alaNAZ
153000111129     C*//                   KFLD                    k_alaPRV
153100111129     C*//                   KFLD                    k_alaCAP
153200111129     C*//                   KFLD                    k_alaLOC
153300111129     C*//                   KFLD                    k_alaZNC
153400111129     C*//                   KFLD                    k_alaSPV
153500061027     C*
153600111129     C*//* CHIAVE LETTURA TIALA02L - COMPLETA
153700111129     C*//     KEYala02_c    KLIST
153800111129     C*//                   KFLD                    k_alaDIV
153900111129     C*//                   KFLD                    k_alaARE
154000111129     C*//                   KFLD                    k_alaLNA
154100111129     C*//                   KFLD                    k_alaZNC
154200111129     C*//                   KFLD                    k_alaNAZ
154300111129     C*//                   KFLD                    k_alaPRV
154400111129     C*//                   KFLD                    k_alaCAP
154500111129     C*//                   KFLD                    k_alaLOC
154600111129     C*//                   KFLD                    k_alaSPV
154700111129      *
154800111129      * - Reperimento dati job
154900111129     c                   exsr      sr_DatiJob
155000061030     C*
155100061030     C* CALCOLA LA DATA CORRENTE
155200061030     C                   time                    wn14             14 0
155300061030     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
155400061030     C                   z-add     wn8           g08dat
155500061030     C                   z-add     *zeros        g08inv
155600061030     C                   movel     '0'           g08err
155700061030     C                   call      'XSRDA8'
155800061030     C                   parm                    wlbda8
155900061030     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
156000061030     C*
156100061030     C* Reperisco la versione cappario alla data corrente
156200061030     C                   z-add     *zeros        o96ver            5 0
156300061030     C                   z-add     datcor        i96dri            8 0
156400061030     C                   call      'TISI96R'
156500061030     C                   parm                    i96dri
156600061030     C                   parm                    o96ver
156700000000     C*--------------------
156800060904     C* CAMPI DI CONTROLLO
156900000000     C*--------------------
157000000000     C                   MOVEL     '0'           FINL00            1
157100060904     C                   SETON                                        01
157200000000     C*
157300000000     C                   ENDSR
157400111129
157500111129      /free
157600111129
157700111129       //--------------------------------------------------------------
157800111129       //?Reperimento Dati del job (Utente/Operativi).                 ?
157900111129       //--------------------------------------------------------------
158000111129       BEGSR  sr_DatiJob;
158100111129
158200111129         in(e) §AzUte;
158300111129         if NOT %error;
158400111129           in(e) §DatiUte;
158500111129         endif;
158600111129         if %error or RSut = *blank;
158700111129           tibs34r ( tibs34ds );
158800111129           in §AzUte;
158900111129           in §DatiUte;
159000111129         endif;
159100111129
159200111129       ENDSR;
159300111129
159400111129       //--------------------------------------------------------------
159500111129       //?Stampa segnalazione dell'errore rilevato.                    ?
159600111129       //--------------------------------------------------------------
159700111129       BEGSR  sr_PrintErr;
159800111129
159900111129         // -?Stampa del Dump?
160000111129         Dump(A);
160100111129
160200111129         // -?Stampa del Job-Log?
160300111129         Qcmd = 'DSPJOBLOG job(*) output(*print)';
160400111129         exsr  sr_ExecCmd;
160500111129
160600111129       ENDSR;
160700111129
160800111129       //--------------------------------------------------------------
160900111129       //?Esecuzione del comando (già impostato).                      ?
161000111129       //--------------------------------------------------------------
161100111129       BEGSR  sr_ExecCmd;
161200111129
161300111129         clear Qcap0100;
161400111129         Qcabcsdh = *off;
161500111129         Qcapa    = *off;
161600111129         Qcacmdss = *off;
161700111129         Qcaerved = *allX'00';
161800111129
161900111129         clear Qusec;
162000111129         Qusbprv  = %size(Qusec);
162100111129
162200111129         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
162300111129                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
162400111129                           0 : 0 : Qusec);
162500111129
162600111129         //if  Qusei <> *blank;
162700111129         //  ...
162800111129         //endif;
162900111129
163000111129       ENDSR;
163100111129
163200111129      /end-free
