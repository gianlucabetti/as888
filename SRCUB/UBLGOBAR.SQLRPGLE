000100970526     H*------------------------------------------------------------------------*
000200161110     H DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP('QILE')
000300161110     H bnddir('UBBNDDIR')
000400970526     F*------------------------------------------------------------------------*
000500970526     F* DATA BASE
000600970526     F*------------------------------------------------------------------------*
000700170201     FUBLGOBAD  CF   E             WORKSTN INFDS(DEVDS1)
000800170201     F                                     SFILE(LGOBD05:SFLULT)
000900170201     F                                     SFILE(LGOBDMS:MSGULT)
001000970526     D*------------------------------------------------------------------------*
001100970526     D* SCHIERE
001200970526     D*------------------------------------------------------------------------*
001300970526     D*------------
001400970526     D* DECODIFICHE
001500970526     D*------------
001600970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
001700970526     D*------------
001800970526     D* MESSAGGI
001900970526     D*------------
002000970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002100970526     D*------------
002200970526     D* COMANDI
002300970526     D*------------
002400970526     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002500970526     D*------------
002600970526     D* DESCRIZIONI OPZIONI
002700970526     D*------------
002800970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
002900970526     D*------------
003000970526     D* DESCRIZIONI TASTI FUNZIONALI
003100970526     D*------------
003200970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003300970526     D*------------
003400970526     D* OPZIONI DA CARICARE A VIDEO
003500970526     D*------------
003600970526     D OPV             S             19    DIM(24)
003700970526     D*------------
003800970526     D* FUNZIONI DA CARICARE A VIDEO
003900970526     D*------------
004000970526     D FUV             S             19    DIM(24)
004100970526     D*------------------------------------------------------------------------*
004200970526     D* INPUT
004300970526     D*------------------------------------------------------------------------*
004400970526     D*------------
004500970526     D* RIDEFINIZIONE OPZIONI VIDEO
004600970526     D*------------
004700970526     D OPDDS           DS
004800970526     D  OPDOPZ                 1      2
004900970526     D  OPDDE1                 3     19
005000970526     D  OPDOP1                21     22
005100970526     D  OPDFI0                23     34
005200970526     D  OPDATT                35     35
005300970526     D  OPDFI1                36     80
005400970526     D  OPDDES                 1     19
005500970526     D  OPDRIG                 1     80
005600970526     D*------------
005700970526     D* RIDEFINIZIONE FUNZIONI VIDEO
005800970526     D*------------
005900970526     D FUDDS           DS
006000970526     D  FUDDE1                 1      1
006100970526     D  FUDFUN                 2      3
006200970526     D  FUDDE2                 4     19
006300970526     D  FUDFI0                20     34
006400970526     D  FUDATT                35     35
006500970526     D  FUDFI1                36     80
006600970526     D  FUDDES                 1     19
006700970526     D  FUDRIG                 1     80
006800970526     D*------------
006900970526     D* DS GESTIONE TASTI FUNZIONALI
007000970526     D*------------
007100970526     D DSKEY         E DS
007200970526     D DEVDS1          DS
007300970526     D  KEY                  369    369
007400970526     D*------------
007500970526     D* DS "XMSG" - GESTIONE MESSAGGI
007600970526     D*------------
007700970526     D DSMSG         E DS
007800970526     D*------------
007900020919     D* DS REPERIMENTO DATI UTENTE
008000970526     D*------------
008100020919     D TIBS34DS      E DS                                                       *Profili utente
008200020919     D DDATIUTE      E DS                                                       *Dati utente
008300020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008400970526     D*------------------
008500970526     D* DS "XSRDA8" - CONTROLLA DATA (8)
008600970526     D*------------------
008700970526     D WLBDA8          DS                  INZ
008800970526     D  G08DAT                 1      8  0
008900970526     D  G08INV                 9     16  0
009000970526     D  G08ERR                17     17
009100970526     D  G08TGI                18     22  0
009200970526     D*------------
009300970526     D* DS DI PROCEDURA
009400970526     D*------------
009500170201     D UBLGOBADS     E DS
009600170201     D WUBLGOBADS      DS                  LIKEDS(UBLGOBADS)
009700170301     D UBLGOBA01S    E DS
009800170323     D UBLGOBA02S    E DS
009900970526     D*------------
010000970526     D* ARCHITETTURA
010100970526     D*------------
010200970526     D KPJBA         E DS
010300970715     D  UTENTE                 1      3
010400160713     D*------------
010500160713     D* campi di appoggio
010600160713     D*------------
010700170302     D DEPLivDrill     S              1s 0 inz(1)
010800170202     D DEPLogica       S                   inz like(D03Logica)
010900170202     D DEPLib          S                   inz like(D03Lib   )
011000170202     D DEPObjT         S                   inz like(D03ObjT  )
011100170202     D DEPObj          S                   inz like(D03Obj   )
011200170202     D DEPAttr         S                   inz like(D03Attr  )
011300170306     D DEPUtC          S                   inz like(D03UtC   )
011400170306     D DEPDati         S                   inz like(D03Dati  )
011500170202     d wLivDrill       s              1s 0 inz(1)
011600170202     d wTipRi          s              3
011700170302     d wExfmt          s              1
011800170328     d GOBB_Error      s              1
011900170329     d GOBB_Msg        s            132
012000170329     d wRichiesta      s              2
012100161111
012200160831       // - Stringa SQL da eseguire
012300160831     d wSQl            s           1024    inz  varying
012400170201     d WAFOB_fetch1    ds                  qualified
012500170201     d  Logica                        2a
012600170202     d  DtEstr                         d
012700170201     d  Lib                          10a
012800170201     d  ObjT                          7a
012900170201     d  Obj                          10a
013000170201     d  Attr                         10a
013100170505     d  ObjD                         50a
013200170201     d  Dati                        130a
013300170323
013400170323       // -?Tasti funzionali a video?
013500170323     d c_F01           c                   const(x'31')
013600170323     d c_F02           c                   const(x'32')
013700170323     d c_F03           c                   const(x'33')
013800170323     d c_F04           c                   const(x'34')
013900170323     d c_F05           c                   const(x'35')
014000170323     d c_F06           c                   const(x'36')
014100170323     d c_F07           c                   const(x'37')
014200170323     d c_F08           c                   const(x'38')
014300170323     d c_F09           c                   const(x'39')
014400170323     d c_F10           c                   const(x'3A')
014500170323     d c_F11           c                   const(x'3B')
014600170323     d c_F12           c                   const(x'3C')
014700170323     d c_F13           c                   const(x'B1')
014800170323     d c_F14           c                   const(x'B2')
014900170323     d c_F15           c                   const(x'B3')
015000170323     d c_F16           c                   const(x'B4')
015100170323     d c_F17           c                   const(x'B5')
015200170323     d c_F18           c                   const(x'B6')
015300170323     d c_F19           c                   const(x'B7')
015400170323     d c_F20           c                   const(x'B8')
015500170323     d c_F21           c                   const(x'B9')
015600170323     d c_F22           c                   const(x'BA')
015700170323     d c_F23           c                   const(x'BB')
015800170323     d c_F24           c                   const(x'BC')
015900170323     d c_Enter         c                   const(x'F1')
016000170323     d c_RollDown      c                   const(x'F4')
016100170323     d c_RollUp        c                   const(x'F5')
016200170323
016300170323       // - Parametri gestione errori API
016400170323      /copy qSysInc/qRpgleSrc,QUSEC
016500170323
016600170323       // Parametri API QCAPCMD (Process Commands)?
016700170323     d Qcmd            s           1024    inz  varying
016800170323      /copy qSysInc/qRpgleSrc,QCAPCMD
016900170323      /copy gaitrasrc/srcProtoPR,QCAPCMD
017000170329
017100170329       // Reperimento NETA sistema AS/400 corrente?
017200170329     d currSysNeta     s              8a   inz
017300170329      /copy gaitrasrc/srcProtoPr,UBRTVNETA
017400161110
017500970526     C*------------------------------------------------------------------------*
017600970526     C* MAIN LINES
017700970526     C*------------------------------------------------------------------------*
017800970526     C*
017900970526     C* SE NON CI SONO ERRORI, PROSEGUE
018000970526IF  1C     ERRL00        IFNE      '1'
018100970526     C*
018200970526     C* CARICA IL SFILE
018300970526     C                   EXSR      POSSFL
018400970526     C*
018500970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
018600970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
018700970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
018800970526     C     FINL00        OREQ      '1'                                          *FINE PGM
018900970526     C*
019000970526     C* REIMPOSTA ERRORE DI PROCERURA
019100970526     C                   MOVEL     '0'           ERRL00
019200970526     C*
019300970526     C* EMETTE IL SFILE
019400970526     C                   EXSR      OUTSFL
019500970526     C*
019600970526     C* GESTIONE TASTI FUNZIONALI
019700970526     C                   EXSR      FUNCON
019800970526     C*
019900970526E   2C                   ENDDO
020000970526E   1C                   ENDIF
020100970526     C* FINE PGM
020200970526     C                   SETON                                        LR
020300970526     C*------------------------------------------------------------------------*
020400970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
020500970526     C*------------------------------------------------------------------------*
020600970526     C     F03GES        BEGSR
020700970526     C*
020800970526     C* IMPOSTA "PREMUTO F03"
020900970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
021000170202     C                   MOVEL     '1'           GOBAF03                         *DI PROCEDURA
021100160916     C*
021200970526     C                   ENDSR
021300970526     C*------------------------------------------------------------------------*
021400970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
021500970526     C*------------------------------------------------------------------------*
021600970526     C     F05GES        BEGSR
021700160923     C*
021800970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
021900970526     C*
022000970526     C                   ENDSR
022100970526     C*------------------------------------------------------------------------*
022200970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
022300970526     C*------------------------------------------------------------------------*
022400970526     C     F06GES        BEGSR
022500970526     C*
022600970526     C* INSERIMENTO CONSENTITO SOLO IN SEDE
022700020919IF  1C     DUTLPO        IFNE      'S'
022800970526     C                   MOVEL     '1'           ERRL00
022900170323     C                   MOVEL     MSG(20)       DSMSMS
023000970526E   1C                   ENDIF
023100970526     C*
023200970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
023300161108     C***                CLEAR                   DPROC
023400970529     C                   MOVEL     DEPOP0        WOP0
023500970529     C                   MOVEL     'F06'         WOP1
023600170323     C                   MOVEL     'XXXXXXXXX'   WPGM             10
023700970526     C                   EXSR      LANPGM                                       *LANCIA PGM
023800970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
023900970526E   1C                   ENDIF
024000970526     C*
024100970526     C                   ENDSR
024200161130     C*------------------------------------------------------------------------*
024300161201     C* F08GES - GESTIONE F08 -> Drill-up
024400161130     C*------------------------------------------------------------------------*
024500161201     C     F08GES        BEGSR
024600161130     C*
024700161130     C                   if        wLivDrill= 1
024800161130     C                   MOVEL     '1'           ERRL00
024900161201     C                   MOVEL     MSG(13)       DSMSMS
025000161130     C                   LEAVESR
025100161130     C                   endif
025200161130
025300161130     C* diminuisce il livello di Drill
025400161130     C                   eval      wLivDrill -= 1
025500161130     C*
025600161130     c                   eval      FUNGru = 0
025700161130     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
025800161130     C*
025900161130     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
026000161130     C                   EXSR      NEWPOS
026100161130     C*
026200161130     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
026300161130IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026400161130IF  2C     SWPOSI        IFNE      0
026500161130     C                   EXSR      POSSFL                                       *RICARICA SFILE
026600161130X   2C                   ELSE
026700161130IF  3C     SFLULT        IFNE      0
026800161130     C                   EXSR      ENTATT                                       *GESTIONE ENTER
026900161130     C*
027000161130     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
027100161130IF  4C     SWMODI        IFNE      0
027200161130     C                   EXSR      POSSFL
027300161130E   4C                   ENDIF
027400161130E   3C                   ENDIF
027500161130E   2C                   ENDIF
027600161130E   1C                   ENDIF
027700161130     C*
027800161130     C                   ENDSR
027900161111     C*------------------------------------------------------------------------*
028000161201     C* F07GES - GESTIONE F07 -> Drill-down
028100161111     C*------------------------------------------------------------------------*
028200161201     C     F07GES        BEGSR
028300161111     C*
028400170201     C                   if        wLivDrill= 99
028500161130     C                   MOVEL     '1'           ERRL00
028600161201     C                   MOVEL     MSG(14)       DSMSMS
028700161130     C                   LEAVESR
028800161130     C                   endif
028900161130
029000161130     C* aumento il livello di Drill
029100161130     C                   eval      wLivDrill += 1
029200161111     C*
029300161111     c                   eval      FUNGru = 0
029400161111     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
029500161111     C*
029600161111     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
029700161111     C                   EXSR      NEWPOS
029800161111     C*
029900161111     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
030000161111IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030100161111IF  2C     SWPOSI        IFNE      0
030200161111     C                   EXSR      POSSFL                                       *RICARICA SFILE
030300161111X   2C                   ELSE
030400161111IF  3C     SFLULT        IFNE      0
030500161111     C                   EXSR      ENTATT                                       *GESTIONE ENTER
030600161111     C*
030700161111     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
030800161111IF  4C     SWMODI        IFNE      0
030900161111     C                   EXSR      POSSFL
031000161111E   4C                   ENDIF
031100161111E   3C                   ENDIF
031200161111E   2C                   ENDIF
031300161111E   1C                   ENDIF
031400161111     C*
031500161111     C                   ENDSR
031600970526     C*------------------------------------------------------------------------*
031700970526     C* F12GES - GESTIONE F12 -> RITORNO
031800970526     C*------------------------------------------------------------------------*
031900970526     C     F12GES        BEGSR
032000970526     C*
032100970526     C* IMPOSTA "PREMUTO F12"
032200970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
032300170202     C                   MOVEL     '1'           GOBAF12                         *DI PROCEDURA
032400970526     C*
032500970526     C                   ENDSR
032600170328     C*------------------------------------------------------------------------*
032700170328     C* F22GES - GESTIONE F22 -> Stampa selezione
032800170328     C*------------------------------------------------------------------------*
032900170328     C     F22GES        BEGSR
033000170328     C*
033100170328     C                   CALL      'UBLGOBBR'
033200170328     C                   PARM                    WSQL
033300170328     C                   PARM                    KNMUS
033400170328     C                   PARM                    GOBB_Error
033500170329     C                   PARM                    GOBB_Msg
033600170328
033700170328             select;
033800170328              when GOBB_Error = '0';
033900170328               // EMISSIONE MESSAGGIO DI AVVERTIMENTO
034000170328               DSMSMS = %trim(MSG(16));
034100170328              when GOBB_Error = '1';
034200170329               //DSMSMS = 'UBLGOBBR - Errore SQL (ma la stampa è stata fatta)';
034300170329               DSMSMS = GOBB_Msg;
034400170328              when GOBB_Error = '2';
034500170329               //DSMSMS = 'UBLGOBBR - Nessun record con lo statement SQL +
034600170329               //          (ma la stampa è stata fatta)';
034700170329               DSMSMS = GOBB_Msg;
034800170328             endsl;
034900170328     C*
035000170328     C                   ENDSR
035100970526     C*------------------------------------------------------------------------*
035200970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
035300970526     C*------------------------------------------------------------------------*
035400970526     C     F23GES        BEGSR
035500970526     C*
035600970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
035700970526     C*
035800970526     C                   ENDSR
035900970526     C*------------------------------------------------------------------------*
036000970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
036100970526     C*------------------------------------------------------------------------*
036200970526     C     F24GES        BEGSR
036300970526     C*
036400970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
036500970526     C*
036600970526     C                   ENDSR
036700970526     C*------------------------------------------------------------------------*
036800970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
036900970526     C*------------------------------------------------------------------------*
037000970526     C     RUPGES        BEGSR
037100970526     C*
037200970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
037300970526     C*
037400970526     C                   ENDSR
037500970526     C*------------------------------------------------------------------------*
037600970526     C* ENTGES - GESTIONE ENTER -> INVIO
037700970526     C*------------------------------------------------------------------------*
037800970526     C     ENTGES        BEGSR
037900970526     C*
038000970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
038100970526     C                   EXSR      NEWPOS
038200970526     C*
038300970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
038400970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
038500970526IF  2C     SWPOSI        IFNE      0
038600970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
038700970526X   2C                   ELSE
038800970526IF  3C     SFLULT        IFNE      0
038900970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
039000970526     C*
039100970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
039200970526IF  4C     SWMODI        IFNE      0
039300970526     C                   EXSR      POSSFL
039400970526E   4C                   ENDIF
039500970526E   3C                   ENDIF
039600970526E   2C                   ENDIF
039700970526E   1C                   ENDIF
039800970526     C*
039900970526     C                   ENDSR
040000970526     C*------------------------------------------------------------------------*
040100970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
040200970526     C*------------------------------------------------------------------------*
040300970526     C     O01GES        BEGSR
040400970526     C*
040500160923     C*
040600160923     C* simulo l'enter
040700160923     C                   exsr      NEWPOS
040800160923     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
040900160923     C*
041000970526     C                   ENDSR
041100970526     C*------------------------------------------------------------------------*
041200970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
041300970526     C*------------------------------------------------------------------------*
041400970526     C     O02GES        BEGSR
041500160927     C*
041600160927     C*
041700161108     C                   ENDSR
041800970526     C*------------------------------------------------------------------------*
041900161108     C* O03GES - GESTIONE OPZIONE 3=COPIA
042000970526     C*------------------------------------------------------------------------*
042100161108     C     O03GES        BEGSR
042200160927     C*
042300170327
042400170329         // Se non ho già fatto la richiesta di dati per la stessa opzione al giro precedente
042500170329         if wRichiesta <> '03';
042600170329           // Emissione     ?window di richiesta libreria di arrivo
042700170329           *in45 = *on;
042800170329           *in46 = *off;
042900170329           *in47 = *on;
043000170329           clear LGOBW01;
043100170329           clear W01txt2 ;
043200170329           clear W01txt3 ;
043300170329           W01txt1 = 'In che libreria copio l''oggetto?';
043400170329           dou Key = c_F06
043500170329            or Key = c_F12;
043600170329             exfmt LGOBW01;
043700170329             if Key = c_F12;
043800170329               leavesr;
043900170329             endif;
044000170329           enddo;
044100170329           wRichiesta = '03';
044200170329         else;
044300170329           Key = c_F06;
044400170329         endif;
044500170327
044600170327           if Key = c_F06;
044700170327             QCmd =
044800170327              'CRTDUPOBJ OBJ(' + %trim(D05Obj) + ')';
044900170327             QCmd = %trim(QCmd) +
045000170327              ' FROMLIB(' + %trim(D05Lib) ;
045100170327             QCmd = %trim(QCmd) + ') OBJTYPE(' + %trim(D05ObjT);
045200170327             QCmd = %trim(QCmd) + ') TOLIB(' + %trim(W01Txt2) + ')';
045300170327
045400170327             // copio oggetto sul sistema
045500170327             exsr sr_execCmd;
045600170327             if  Qusei <> *blank;
045700170327               OpzOK = 1;
045800170327               // EMISSIONE MESSAGGIO DI ERRORE
045900170327               DSMSMS = %trim(MSG(3)) + ' ' + %trim(QCmd);
046000170327             else;
046100170327               // EMISSIONE MESSAGGIO DI AVVERTIMENTO
046200170327               DSMSMS = %trim(MSG(15));
046300170327             endif;
046400170327           endif;
046500170327
046600970526     C*
046700970526     C                   ENDSR
046800161108     C*------------------------------------------------------------------------*
046900161108     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
047000161108     C*------------------------------------------------------------------------*
047100161108     C     O04GES        BEGSR
047200161108     C*
047300170323
047400170323         // Emissione window?di richiesta conferma
047500170327         *in45 = *on;
047600170327         *in46 = *on;
047700170327         *in47 = *on;
047800170323         clear LGOBW01;
047900170323         clear W01txt1 ;
048000170323         clear W01txt3 ;
048100170524         W01txt2 = 'Confermi l''eliminazione dell''oggetto ' +
048200170524                   %trim(D05Obj) + '?';
048300170323         dou Key = c_F06
048400170323          or Key = c_F12;
048500170323         exfmt LGOBW01;
048600170323
048700170323           if Key = c_F06;
048800170323             QCmd =
048900170323              'DLTOBJ OBJ(' + %trim(D05Lib) ;
049000170323             QCmd = %trim(QCmd) + '/' + %trim(D05Obj);
049100170323             QCmd = %trim(QCmd) + ') OBJTYPE(' + %trim(D05ObjT);
049200170323             QCmd = %trim(QCmd) + ')';
049300170323
049400170323             // cancello oggetto sul sistema
049500170323             exsr sr_execCmd;
049600170323             if  Qusei <> *blank;
049700170323               OpzOK = 1;
049800170323               // EMISSIONE MESSAGGIO DI ERRORE
049900170323               DSMSMS = %trim(MSG(3)) + ' ' + %trim(QCmd);
050000170323             else;
050100170323
050200170323               // cancello il rcd su WAFOB
050300170323               QCmd =
050400170323                'delete from WAFOB00F +
050500170323                 where +
050600170323                 FOBDTESTR = ''' + %char(%date(D05DtEstr:*eur)) + ''' and +
050700170323                 FOBLOGICA = ''' + %trim(D05Logica) + ''' and +
050800170323                 FOBOBJ    = ''' + %trim(D05Obj) + ''' and +
050900170323                 FOBLIB    = ''' + %trim(D05Lib) + ''' and +
051000170323                 FOBOBJT   = ''' + %trim(D05ObjT) + '''';
051100170323
051200170323               exec sql execute immediate :QCmd;
051300170323               if SQLCod < 0 or SQLCod = 100;
051400170323                 OpzOK = 1;
051500170323                 // EMISSIONE MESSAGGIO DI ERRORE
051600170323                 DSMSMS = %trim(MSG(3)) + ' ' + %trim(QCmd);
051700170323               endif;
051800170323
051900170323             endif;
052000170323           endif;
052100170323
052200170323         enddo;
052300161108     C*
052400161108     C                   ENDSR
052500970526     C*------------------------------------------------------------------------*
052600970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
052700970526     C*------------------------------------------------------------------------*
052800970526     C     O05GES        BEGSR
052900970526     C*
053000170301     C                   MOVEL     DEPOP0        WOP0
053100170301     C                   MOVEL     'O05'         WOP1
053200170301     C                   select
053300170301     C                   when      D05Logica = '01'
053400170301     C                   EVAL      BA01Logica     = D05Logica
053500170301     C                   EVAL      BA01Obj        = D05Obj
053600170301     C                   EVAL      BA01Lib        = D05Lib
053700170301     C                   EVAL      BA01ObjT       = D05ObjT
053800170307     C                   EVAL      BA01DTESTR     = D05DTESTRI
053900170301     C                   MOVEL     'UBLGOBA01R'  WPGM
054000170323     C*
054100170323     C                   when      D05Logica = '02'
054200170323     C                   EVAL      BA02Logica     = D05Logica
054300170323     C                   EVAL      BA02Obj        = D05Obj
054400170323     C                   EVAL      BA02Lib        = D05Lib
054500170323     C                   EVAL      BA02ObjT       = D05ObjT
054600170323     C                   EVAL      BA02DTESTR     = D05DTESTRI
054700170323     C                   MOVEL     'UBLGOBA02R'  WPGM
054800170301     C                   endsl
054900170301     C                   EXSR      LANPGM
055000970526     C*
055100970526     C                   ENDSR
055200170327     C*------------------------------------------------------------------------*
055300170327     C* O11GES - GESTIONE OPZIONE 11=MUOVI
055400170327     C*------------------------------------------------------------------------*
055500170327     C     O11GES        BEGSR
055600170327     C*
055700170327
055800170329         // Se non ho già fatto la richiesta di dati per la stessa opzione al giro precedente
055900170329         if wRichiesta <> '11';
056000170329           // Emissione     ?window di richiesta libreria di arrivo
056100170329           *in45 = *on;
056200170329           *in46 = *off;
056300170329           *in47 = *on;
056400170329           clear LGOBW01;
056500170329           clear W01txt2 ;
056600170329           clear W01txt3 ;
056700170329           W01txt1 = 'In che libreria sposto l''oggetto?';
056800170329           dou Key = c_F06
056900170329            or Key = c_F12;
057000170329             exfmt LGOBW01;
057100170329             if Key = c_F12;
057200170329               leavesr;
057300170329             endif;
057400170329           enddo;
057500170329           wRichiesta = '11';
057600170329         else;
057700170329           Key = c_F06;
057800170329         endif;
057900170327
058000170504         if Key = c_F06;
058100170504           //prima provo a cancellare l'oggetto nella libreria d'arrivo
058200170504           QCmd =
058300170504            'DLTOBJ OBJ(' + %trim(W01Txt2) ;
058400170504           QCmd = %trim(QCmd) + '/' + %trim(D05Obj);
058500170504           QCmd = %trim(QCmd) + ') OBJTYPE(' + %trim(D05ObjT);
058600170504           QCmd = %trim(QCmd) + ')';
058700170504           // cancello oggetto sul sistema
058800170504           exsr sr_execCmd;
058900170504
059000170504           // poi muovo l'oggetto nella libreria d'arrivo (non mi interessa il risultato precedente
059100170504           // se da errore per non trovato)
059200170504           QCmd =
059300170504            'MOVOBJ OBJ(' + %trim(D05Lib) ;
059400170504           QCmd = %trim(QCmd) + '/' + %trim(D05Obj);
059500170504           QCmd = %trim(QCmd) + ') OBJTYPE(' + %trim(D05ObjT);
059600170504           QCmd = %trim(QCmd) + ') TOLIB(' + %trim(W01Txt2) + ')';
059700170327
059800170504           // sposto oggetto sul sistema
059900170504           exsr sr_execCmd;
060000170504           if  Qusei <> *blank;
060100170504             OpzOK = 1;
060200170504             // EMISSIONE MESSAGGIO DI ERRORE
060300170504             DSMSMS = %trim(MSG(3)) + ' ' + %trim(QCmd);
060400170504           else;
060500170504
060600170504             // cancello il rcd su WAFOB
060700170504             QCmd =
060800170504              'delete from WAFOB00F +
060900170504               where +
061000170504               FOBDTESTR = ''' + %char(%date(D05DtEstr:*eur)) + ''' and +
061100170504               FOBLOGICA = ''' + %trim(D05Logica) + ''' and +
061200170504               FOBOBJ    = ''' + %trim(D05Obj) + ''' and +
061300170504               FOBLIB    = ''' + %trim(D05Lib) + ''' and +
061400170504               FOBOBJT   = ''' + %trim(D05ObjT) + '''';
061500170504
061600170504             exec sql execute immediate :QCmd;
061700170504             if SQLCod < 0 or SQLCod = 100;
061800170504               OpzOK = 1;
061900170504               // EMISSIONE MESSAGGIO DI ERRORE
062000170504               DSMSMS = %trim(MSG(3)) + ' ' + %trim(QCmd);
062100170504             else;
062200170504               // EMISSIONE MESSAGGIO DI AVVERTIMENTO
062300170504               DSMSMS = %trim(MSG(15));
062400170504             endif;
062500170504           endif;
062600170504         endif;
062700170329
062800170327     C*
062900170327     C                   ENDSR
063000170504     C*------------------------------------------------------------------------*
063100170504     C* O12GES - GESTIONE OPZIONE 12=Allinea SETRAS
063200170504     C*------------------------------------------------------------------------*
063300170504     C     O12GES        BEGSR
063400170504     C*
063500170504
063600170504         // Se non ho già fatto la richiesta di dati per la stessa opzione al giro precedente
063700170504         if wRichiesta <> '12';
063800170504           // Emissione     ?window di richiesta libreria di arrivo
063900170504           *in45 = *on;
064000170504           *in46 = *off;
064100170504           *in47 = *on;
064200170504           clear LGOBW01;
064300170504           clear W01txt2 ;
064400170504           clear W01txt3 ;
064500170504           W01txt1 = 'In che libreria di SETRAS copio l''oggetto?';
064600170504           W01txt2 = 'UNITRAGRU';
064700170504           dou Key = c_F06
064800170504            or Key = c_F12;
064900170504             exfmt LGOBW01;
065000170504             if Key = c_F12;
065100170504               leavesr;
065200170504             endif;
065300170504           enddo;
065400170504           wRichiesta = '12';
065500170504         else;
065600170504           Key = c_F06;
065700170504         endif;
065800170504
065900170504           if Key = c_F06;
066000170504             QCmd =
066100170504              'SAVRSTOBJ OBJ(' + %trim(D05Obj) ;
066200170504             QCmd = %trim(QCmd) + ') LIB(' + %trim(D05Lib);
066300170504             QCmd = %trim(QCmd) + ') RMTLOCNAME(SETRAS';
066400170504             QCmd = %trim(QCmd) + ') OBJTYPE(' + %trim(D05ObjT);
066500170504             QCmd = %trim(QCmd) + ') RSTLIB(' + %trim(W01Txt2) + ')';
066600170504
066700170504             // sposto oggetto sul sistema
066800170504             exsr sr_execCmd;
066900170504             if  Qusei <> *blank;
067000170504               OpzOK = 1;
067100170504               // EMISSIONE MESSAGGIO DI ERRORE
067200170504               DSMSMS = %trim(MSG(3)) + ' ' + %trim(QCmd);
067300170504             else;
067400170504               // EMISSIONE MESSAGGIO DI AVVERTIMENTO
067500170504               DSMSMS = %trim(MSG(15));
067600170504             endif;
067700170504           endif;
067800170504
067900170504     C*
068000170504     C                   ENDSR
068100970526     C*------------------------------------------------------------------------*
068200170302     C* O14GES - Storicizzazione
068300970526     C*------------------------------------------------------------------------*
068400170302     C     O14GES        BEGSR
068500970526     C*
068600170307     C                   CALL      'UBLGOBSTR'
068700170307     C                   PARM                    D05DtEstrI
068800170307     C                   PARM                    D05Logica
068900170307     C                   PARM                    D05Obj
069000170302     C                   PARM                    D05Lib
069100170302     C                   PARM                    D05ObjT
069200170303     C                   PARM                    D05Dati
069300170302     C                   PARM                    wExfmt
069400170302     C*
069500170302     C* tolgo memo che c'è stata una exfmt del sfl
069600170302     C                   EVAL      wExfmt = *off
069700970526     C*
069800970526     C                   ENDSR
069900970526     C*------------------------------------------------------------------------*
070000970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
070100970526     C*------------------------------------------------------------------------*
070200970526     C     NEWPOS        BEGSR
070300970526     C*
070400970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
070500970526     C*         1= RICHIESTO POSIZIONAMENTO
070600970526     C                   Z-ADD     0             SWPOSI            7 0
070700970526     C*
070800970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
070900970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
071000970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
071100970526     C                   Z-ADD     0             SWMODI            1 0
071200970526     C*
071300970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
071400161130     C     wLivDrill     IFNE      DEPLivDrill
071500170201     C     D03Logica     ORNE      DEPLogica
071600170201     C     D03Lib        ORNE      DEPLib
071700170201     C     D03ObjT       ORNE      DEPObjT
071800170201     C     D03Obj        ORNE      DEPObj
071900170201     C     D03Attr       ORNE      DEPAttr
072000170306     C     D03UtC        ORNE      DEPUtC
072100170306     C     D03Dati       ORNE      DEPDati
072200970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
072300970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
072400970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
072500161130     C                   EVAL      DEPLivDrill = wLivDrill
072600170202     C                   EVAL      DEPLogica = D03Logica
072700170202     C                   EVAL      DEPLib    = D03Lib
072800170202     C                   EVAL      DEPObjT   = D03ObjT
072900170202     C                   EVAL      DEPObj    = D03Obj
073000170202     C                   EVAL      DEPAttr   = D03Attr
073100170306     C                   EVAL      DEPUtC    = D03UtC
073200170306     C                   EVAL      DEPDati   = D03Dati
073300970526E   2C                   ENDIF
073400970526E   1C                   ENDIF
073500970526     C*
073600970526     C                   ENDSR
073700970526     C*------------------------------------------------------------------------*
073800970526     C* SETFIL - POSIZIONAMENTO RECORD
073900970526     C*------------------------------------------------------------------------*
074000970526     C     SETFIL        BEGSR
074100970526     C*
074200970526     C* POSIZIONAMENTO DEL FILE E PRIMA LETTURA IN BASE AL TIPO DI RICERCA
074300160830     C                   SELECT
074400160831     C*
074500161108IF  1C     WTIPRI        WHENEQ    *blank
074600160831     C* i dati li reperisce mediante SQL
074700160831     C                   EXSR      Prepara_SQL
074800160830     C*
074900160830E   1C                   ENDSL
075000970526     C* LETTURA RECORD
075100970526IF  1C     *IN99         IFEQ      *OFF
075200970526     C                   EXSR      LETFIL
075300970526E   1C                   ENDIF
075400970526     C*
075500970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
075600970526IF  1C     *IN99         IFEQ      *ON
075700970526     C                   Z-ADD     1             SFLEOF
075800970526E   1C                   ENDIF
075900970526     C*
076000970526     C                   ENDSR
076100970526     C*------------------------------------------------------------------------*
076200970526     C* LETFIL - LETTURA RECORD
076300970526     C*------------------------------------------------------------------------*
076400970526     C     LETFIL        BEGSR
076500970526     C*
076600970526     C* LEGGE FINO A:
076700970526     C*     - FINE FILE
076800970526     C*     - TROVATO RECORD VALIDO
076900970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
077000970526DO  1C     *IN99         DOWEQ     *OFF
077100970526     C     WRECOK        ANDEQ     'N'
077200970526     C*
077300970526     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO DI RICERCA
077400160830     C                   SELECT
077500160831     C*
077600161108IF  1C     WTIPRI        WHENEQ    *blank
077700160831     C* i dati li reperisce mediante SQL
077800160901     C                   EXSR      Legge_SQL
077900160830     C*
078000160830E   2C                   ENDSL
078100970526     C*
078200970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
078300970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
078400970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
078500970530X   2C                   ELSE
078600970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
078700970530E   2C                   ENDIF
078800970526E   1C                   ENDDO
078900970526     C*
079000970526     C                   ENDSR
079100970526     C*------------------------------------------------------------------------*
079200970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
079300970526     C*------------------------------------------------------------------------*
079400970526     C     CHKREC        BEGSR
079500970526     C*
079600970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
079700160913     C*
079800160919     C* Proc diversa
079900161108IF  1C*                  if        wDrillUp  =  *off and
080000161108IF  1C*                            D03Proc<> *blank and
080100161108IF  1C*                            D03Proc<> VMSProc
080200161108     C*                  MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
080300161108     C*                  leavesr
080400161108     C*                  endif
080500161108
080600160713
080700970526     C                   ENDSR
080800970526     C*------------------------------------------------------------------------*
080900160830     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE            §
081000970526     C*------------------------------------------------------------------------*
081100970526     C     IRESFL        BEGSR
081200970526     C*
081300970526     C                   MOVEL     *BLANKS       OPZKEY
081400161111
081500161130         select;
081600161130
081700170201          // tengo la gestione drill anche se PER ORA non è usata
081800170201          when wLivDrill = 1;
081900170201           D05Logica = WAFOB_fetch1.Logica;
082000170202           D05DtEstr = %char(WAFOB_fetch1.DtEstr:*eur);
082100170307           D05DtEstrI = WAFOB_fetch1.DtEstr;
082200170201           D05Lib    = WAFOB_fetch1.Lib;
082300170201           D05ObjT   = WAFOB_fetch1.ObjT;
082400170201           D05Obj    = WAFOB_fetch1.Obj;
082500170201           D05Attr   = WAFOB_fetch1.Attr;
082600170504           D05ObjD   = WAFOB_fetch1.ObjD;
082700170306           D05UtC    = %subst(WAFOB_fetch1.Dati:1:10);
082800170306           D05Dati   = WAFOB_fetch1.Dati;
082900161130
083000161130         endsl;
083100160901     C*
083200970526     C                   ENDSR
083300161115     C*------------------------------------------------------------------------*
083400161115     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
083500161115     C*------------------------------------------------------------------------*
083600161115     C     CHKPAR        BEGSR
083700161115     C*
083800970526     C* REIMPOSTO FLAG EVIDENZAZIONE ERRORI SUI PARAMETRI
083900970526     C                   SETOFF                                       808182
084000160913     C                   SETOFF                                       838485
084100970526     C*--------------------
084200970526     C* ORDINAMENTO
084300970526     C*--------------------
084400970526     C*--------------------
084500970526     C* SELEZIONI
084600970526     C*--------------------
084700970526     C*
084800160831     C* SE NESSUN ORDINAMENTO RICHIESTO, utilizzo il caricamento completo per Procedura
084900160830     C*
085000161108     C                   EVAL      WTIPRI = *blank
085100160830IF  1C     ERRL00        IFEQ      '0'
085200161108     C*                  SELECT
085300161108E   2C*                  ENDSL
085400970526E   1C                   ENDIF
085500970526     C*
085600970526     C                   ENDSR
085700970526     C*------------------------------------------------------------------------*
085800970526     C* LANPGM - LANCIA UN PROGRAMMA
085900970526     C*------------------------------------------------------------------------*
086000970526     C     LANPGM        BEGSR
086100970526     C*
086200970526     C* IMPOSTA LA DS DI PROCEDURA
086300170301     C                   select
086400170301     C                   when      D05Logica = '01'
086500170301     C                   MOVEL     WOP0          BA01OP0                         *LIVELLO PROCEDURA
086600170301     C                   MOVEL     WOP1          BA01OP1                         *LIVELLO PGM
086700170301     C                   MOVEL     '0'           BA01F03                         *PREMUTO F03=FINE
086800170301     C                   MOVEL     '0'           BA01F12                         *PREMUTO F12=RIT
086900170301     C                   MOVEL     '0'           BA01ERR                         *'1'=ERRORE
087000170301     C                   MOVEL     *BLANKS       BA01MSG                         *MESSAGGIO DI RIT
087100970526     C*
087200970526     C                   CALL      WPGM
087300970612     C                   PARM                    KPJBA
087400170301     C                   PARM                    UBLGOBA01S
087500970526     C*
087600970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
087700170202IF  1C     GOBAF03       IFEQ      '1'                                          *PREMUTO F03
087800970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
087900970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
088000970526X   1C                   ELSE                                                   *NON PREMUTO F03
088100170301     C                   MOVEL     BA01MSG       DSMSMS                         *MESSAGGIO DI RIT
088200970526E   1C                   ENDIF
088300970526     C*
088400170323     C                   when      D05Logica = '02'
088500170323     C                   MOVEL     WOP0          BA02OP0                         *LIVELLO PROCEDURA
088600170323     C                   MOVEL     WOP1          BA02OP1                         *LIVELLO PGM
088700170323     C                   MOVEL     '0'           BA02F03                         *PREMUTO F03=FINE
088800170323     C                   MOVEL     '0'           BA02F12                         *PREMUTO F12=RIT
088900170323     C                   MOVEL     '0'           BA02ERR                         *'1'=ERRORE
089000170323     C                   MOVEL     *BLANKS       BA02MSG                         *MESSAGGIO DI RIT
089100170323     C*
089200170323     C                   CALL      WPGM
089300170323     C                   PARM                    KPJBA
089400170323     C                   PARM                    UBLGOBA02S
089500170323     C*
089600170323     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
089700170323IF  1C     GOBAF03       IFEQ      '1'                                          *PREMUTO F03
089800170323     C                   MOVEL     '1'           ERRL00                         *ERRORE
089900170323     C                   MOVEL     '1'           F03L00                         *FINE PGM
090000170323X   1C                   ELSE                                                   *NON PREMUTO F03
090100170323     C                   MOVEL     BA02MSG       DSMSMS                         *MESSAGGIO DI RIT
090200170323E   1C                   ENDIF
090300170323     C*
090400170301     C                   ENDSL
090500170301     C*
090600970526     C                   ENDSR
090700970527     C*------------------------------------------------------------------------*
090800970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
090900970527     C*------------------------------------------------------------------------*
091000970527     C     CARTAB        BEGSR
091100970527     C*
091200970527     C                   ENDSR
091300970526     C*------------------------------------------------------------------------*
091400970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
091500970526     C*------------------------------------------------------------------------*
091600970526     C     CHKFOP        BEGSR
091700970626     C*
091800970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
091900970626IF  1C     DEPOP0        IFEQ      'R01'
092000161111     C***                MOVEL     OPD(1)        OPDRIG                         *SCELTA
092100161111     C***                MOVEL     'S'           OPDATT
092200161111     C***                MOVEL     OPDRIG        OPD(1)
092300970626X   1C                   ELSE
092400970626     C* OPZIONI
092500170301     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
092600170301     C                   MOVEL     'S'           OPDATT
092700170301     C                   MOVEL     OPDRIG        OPD(5)
092800170302     C                   MOVEL     OPD(14)       OPDRIG                         *INTERROGAZIONE
092900170302     C                   MOVEL     'S'           OPDATT
093000170302     C                   MOVEL     OPDRIG        OPD(14)
093100970626     C* FUNZIONI
093200970626E   1C                   ENDIF
093300970626     C*
093400000602     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
093500970626IF  1C     DEPOP0        IFEQ      'R02'
093600161111     C***                MOVEL     OPD(1)        OPDRIG                         *MODIFICA
093700161111     C***                MOVEL     'S'           OPDATT
093800161111     C***                MOVEL     OPDRIG        OPD(1)
093900161111     C***                MOVEL     OPD(2)        OPDRIG                         *MODIFICA
094000161111     C***                MOVEL     'S'           OPDATT
094100161111     C***                MOVEL     OPDRIG        OPD(2)
094200170329         // attivo solo se sono su AS888
094300170329         if  %subst ( currSysNeta : 1 : 6 ) = 'AS888';
094400170327     C                   MOVEL     OPD(3)        OPDRIG                         *MODIFICA
094500170327     C                   MOVEL     'S'           OPDATT
094600170327     C                   MOVEL     OPDRIG        OPD(3)
094700170327     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
094800170327     C                   MOVEL     'S'           OPDATT
094900170327     C                   MOVEL     OPDRIG        OPD(4)
095000170327     C                   MOVEL     OPD(11)       OPDRIG                         *MODIFICA
095100170327     C                   MOVEL     'S'           OPDATT
095200170327     C                   MOVEL     OPDRIG        OPD(11)
095300170504     C                   MOVEL     OPD(12)       OPDRIG                         *MODIFICA
095400170504     C                   MOVEL     'S'           OPDATT
095500170504     C                   MOVEL     OPDRIG        OPD(12)
095600170329         endif;
095700970715     C*
095800161111     C* SOLO SE EDP ABILITA  INSERIMENTO
095900970715IF  2C     UTENTE        IFEQ      'EDP'
096000170329         // attivo solo se sono su AS888
096100170329         if  %subst ( currSysNeta : 1 : 6 ) = 'AS888';
096200170327     C                   MOVEL     OPD(3)        OPDRIG                         *COPIA
096300170327     C                   MOVEL     'S'           OPDATT
096400170327     C                   MOVEL     OPDRIG        OPD(3)
096500170327     C                   MOVEL     OPD(11)       OPDRIG                         *COPIA
096600170327     C                   MOVEL     'S'           OPDATT
096700170327     C                   MOVEL     OPDRIG        OPD(11)
096800170504     C                   MOVEL     OPD(12)       OPDRIG                         *COPIA
096900170504     C                   MOVEL     'S'           OPDATT
097000170504     C                   MOVEL     OPDRIG        OPD(12)
097100170329         endif;
097200170329     C***                MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
097300170329     C***                MOVEL     'S'           FUDATT
097400170329     C***                MOVEL     FUDRIG        FUD(6)
097500970715E   2C                   ENDIF
097600970715E   1C                   ENDIF
097700970626     C*
097800000602     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO, ALLINEAMENTO
097900020919IF  1C     DUTLPO        IFNE      'S'                                          *FILIALE
098000970626     C                   MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
098100970626     C                   MOVEL     'N'           OPDATT
098200970626     C                   MOVEL     OPDRIG        OPD(4)
098300970626E   1C                   ENDIF
098400970526     C*
098500970526     C* GESTIONE OPZIONI A VIDEO
098600970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
098700970526     C                   EXSR      OPZGES
098800970526     C*
098900970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
099000970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
099100970526     C                   EXSR      FUNGES
099200970526     C*
099300970526     C                   ENDSR
099400020919     C*--------------------------------------------------------------------------------------------*
099500020919     C* REPERISCE I DATI UTENTE
099600020919     C*--------------------------------------------------------------------------------------------*
099700020919     C     REPDATIUTE    BEGSR
099800020919     C*
099900020919     C* INIZIALIZZA VARIABILI DI WRK
100000020919     C                   CLEAR                   TIBS34DS
100100020919     C                   CLEAR                   AZUTEDS
100200020919     C                   CLEAR                   DDATIUTE
100300020919     C*
100400020919     C     *DTAARA       DEFINE    §azute        azuteds
100500020919     C     *DTAARA       DEFINE    §datiute      ddatiute
100600020919     C                   IN(E)     *DTAARA
100700020919if  1C                   IF        %Error
100800020919     c                   EVAL      I34Tla = 'L'
100900020919     C                   CALL      'TIBS34R'
101000020919     C                   PARM                    Tibs34Ds
101100020919     C                   IN        *DTAARA
101200020919e   1C                   ENDIF
101300020919     C*
101400020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
101500020919     C                   MOVEL(P)  RSUT          DSFIRS           20
101600020919     C*
101700020919     C                   ENDSR
101800970526     C*------------------------------------------------------------------------*
101900970526     C* *INZSR - OPERAZIONI INIZIALI
102000970526     C*------------------------------------------------------------------------*
102100970526     C     *INZSR        BEGSR
102200970526     C*---
102300970526     C* RICEVIMENTO PARAMETRI
102400970526     C*---
102500970526     C     *ENTRY        PLIST
102600970612     C                   PARM                    KPJBA
102700160714     C*
102800170201     C                   EVAL      UBLGOBADS = kpjbu
102900160913     c                   if        kpjbu = *blank
103000170201     c                   clear                   UBLGOBADS
103100160913     c                   endif
103200970526     C*
103300970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
103400970526     C     XMSGPA        PLIST
103500970526     C                   PARM                    DSMSMS
103600970526     C                   PARM                    DSMSPG
103700970526     C                   PARM                    DSMSKE
103800970526     C                   PARM                    DSMSER
103900970526     C*
104000970526     C* IMPOSTA LA DS -TASTI PREMUTI-
104100970526     C                   CALL      'XKEY'
104200970526     C                   PARM                    DSKEY
104300970526     C*---
104400970526     C* VARIABILI NON RIFERITE AL DATA BASE
104500970526     C*---
104600970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
104700970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
104800970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
104900970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
105000970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
105100970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
105200970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
105300970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
105400970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
105500170202     C     *LIKE         DEFINE    GOBAOP0       DEPOP0                         *DI DEPOSITO
105600170202     C     *LIKE         DEFINE    GOBAOP1       DEPOP1
105700170202     C     *LIKE         DEFINE    GOBAOP0       WOP0                           *DI LAVORO
105800170202     C     *LIKE         DEFINE    GOBAOP1       WOP1
105900970526     C*---
106000970526     C* VARIABILI DI CONTROLLO
106100970526     C*---
106200970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
106300970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
106400970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
106500970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
106600970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
106700170301     C                   Z-ADD     15            SFLPAG                         *N°RIGHE X PAG
106800970526     C*
106900970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
107000170201     C                   MOVEL     'UBLGOBAR'    DSMSPG
107100170201     C                   MOVEL     'UBLGOBAR'    MSGQUE
107200970526     C                   MOVEL     *BLANKS       MSGKEY
107300970526     C                   MOVEL     *BLANKS       DSMSMS
107400970526     C                   MOVEL     *BLANKS       DSMSKE
107500970526     C                   Z-ADD     *ZEROS        DSMSER
107600970526     C*---
107700970526     C* VARIABILI RIFERITE AL DATA BASE
107800970526     C*---
107900970526     C*---
108000970526     C* CHIAVI DI LETTURA
108100970526     C*---
108200170329
108300170329         // Verifica del sistema AS/400 corrente
108400170329         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
108500170329           *inlr = *on;
108600170329           return;
108700170329         endif;
108800970527     C*
108900970527     C* REPERIMENTO DATI UTENTE
109000020919     C                   EXSR      REPDATIUTE
109100970527     C*
109200970527     C* CARICA LE TABELLE OCCORRENTI
109300970527     C                   EXSR      CARTAB
109400970526     C*
109500970526     C* DEPOSITA LE OPZIONI DI ENTRATA
109600170202     C                   MOVEL     GOBAOP0       DEPOP0                         *LIVELLO PROCEDURA
109700170202     C                   MOVEL     GOBAOP1       DEPOP1                         *LIVELLO PROGRAMMA
109800970526     C*
109900970526     C* CALCOLA LA DATA CORRENTE
110000970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
110100970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
110200970526     C                   Z-ADD     WN8           G08DAT
110300970526     C                   Z-ADD     *ZEROS        G08INV
110400970526     C                   MOVEL     '0'           G08ERR
110500970526     C                   CALL      'XSRDA8'
110600970526     C                   PARM                    WLBDA8
110700970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
110800970526     C*
110900970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
111000970526     C                   EXSR      CHKFOP
111100970526     C*
111200970526     C* IMPOSTA I PARAMETRI DI INGRESSO
111300970526     C*
111400161108     C*
111500970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
111600970530IF  1C     DEPOP0        IFEQ      'R01'
111700970530     C                   MOVEL     DEC(3)        VIDING
111800970530E   1C                   ENDIF
111900970530IF  1C     DEPOP0        IFEQ      'R02'
112000970530     C                   MOVEL     DEC(1)        VIDING
112100970530E   1C                   ENDIF
112200970530IF  1C     DEPOP0        IFEQ      'R05'
112300970530     C                   MOVEL     DEC(2)        VIDING
112400970530E   1C                   ENDIF
112500970526     C*
112600970526     C                   ENDSR
112700161110
112800970526     C*------------------------------------------------------------------------*
112900970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
113000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
113100970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
113200970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
113300970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
113400970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
113500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
113600970526     C*------------------------------------------------------------------------*
113700970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
113800970526     C*------------------------------------------------------------------------*
113900970526     C     POSSFL        BEGSR
114000970526     C*
114100970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
114200970526     C                   EXSR      CLRSFL
114300970526     C*
114400170118     C**************************************************************************
114500170118     C* siccome dentro la routine CARSFL c'è il richiamo alla routine SETFIL se
114600170118     C* non sono stati caricati rcd sul sfl (che vengono caricati dalla routine
114700170118     C* CARSFL dopo il richiamo alla SETFIL),
114800170118     C* non ha senso chiamare prima la SETFIL, faccio fare tutto alla CARSFL
114900170118     C**************************************************************************
115000970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
115100170118     C***                EXSR      SETFIL
115200970526     C*
115300970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
115400170118     C***  SFLEOF        IFEQ      0
115500170118     C***                EXSR      CARSFL
115600170118     C***                END
115700170118     C**************************************************************************
115800170118     C*
115900170118     C                   EXSR      CARSFL
116000970526     C*
116100970526     C                   ENDSR
116200970526     C*------------------------------------------------------------------------*
116300970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
116400970526     C*------------------------------------------------------------------------*
116500970526     C     ENTATT        BEGSR
116600970526     C*
116700970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
116800970526     C                   Z-ADD     0             OPZOK             1 0
116900970526     C*
117000970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
117100970526     C                   Z-ADD     0             SFLVAR            5 0
117200970526     C*
117300970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
117400970526     C                   Z-ADD     0             SFLERR
117500970526     C*
117600970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
117700970526     C                   Z-ADD     SFLULT        WFLULT
117800970526     C*
117900970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
118000170201     C                   READC     LGOBD05                                98
118100970526     C     *IN98         DOWEQ     '0'
118200970526     C*
118300970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
118400970526     C                   ADD       1             SFLVAR
118500970526     C*
118600970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
118700970526     C     OPZOK         IFEQ      0
118800970526     C*
118900970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
119000970526     C     OPZKEY        IFNE      *BLANKS
119100970526     C                   EXSR      OPZCON
119200970526     C                   END
119300970526     C*
119400970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
119500970526     C*            2. PULIZIA SCELTA IN SFL
119600970526     C     OPZOK         IFEQ      0
119700970526     C                   MOVEL     '0'           *IN41
119800970526     C                   MOVEL     *BLANKS       OPZKEY
119900970526     C                   ELSE
120000970526     C*
120100970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
120200970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
120300970526     C                   MOVEL     '1'           *IN41
120400970526     C                   Z-ADD     SFLULT        SFLERR
120500970526     C                   END
120600970526     C                   ELSE
120700970526     C*
120800970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
120900970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
121000970526     C                   MOVEL     '1'           *IN41
121100970526     C                   END                                                    OPZOK=0
121200970526     C*
121300970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
121400970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
121500970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
121600970526     C     OPZOK         IFNE      0
121700970526     C                   Z-ADD     SFLERR        SFLNBR
121800970526     C                   ELSE
121900970526     C                   Z-ADD     SFLULT        SFLNBR
122000970526     C                   END
122100970526     C*
122200970526     C* AGGIORNO RECORD SFL
122300160902     C*  imposto "effetti speciali"
122400160902     C                   EXSR      SFXD05
122500170201     C                   UPDATE    LGOBD05
122600970526     C*
122700170201     C                   READC     LGOBD05                                98
122800970526     C                   END                                                    DO WHILE
122900970526     C*
123000970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
123100970526     C                   Z-ADD     WFLULT        SFLULT
123200970526     C*
123300970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
123400970526     C     SFLVAR        IFEQ      0
123500970526     C*---                 MOVEL'1'       FINL00
123600970526     C                   END
123700970526     C*
123800970526     C                   ENDSR
123900970526     C*------------------------------------------------------------------------*
124000970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
124100970526     C*------------------------------------------------------------------------*
124200970526     C     CLRSFL        BEGSR
124300970526     C*
124400970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
124500970526     C* (0=NON FINE FILE  1=FINE FILE)
124600970526     C                   Z-ADD     0             SFLEOF
124700970526     C*
124800970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
124900970526     C                   Z-ADD     0             SFLULT
125000970526     C*
125100970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
125200970526     C                   MOVEL     '1'           *IN40
125300170201     C                   WRITE     LGOBD06
125400970526     C                   MOVEL     '0'           *IN40
125500970526     C*
125600970526     C                   ENDSR
125700970526     C*------------------------------------------------------------------------*
125800970526     C* CARSFL - CARICAMENTO SFL
125900970526     C*------------------------------------------------------------------------*
126000970526     C     CARSFL        BEGSR
126100970526     C*
126200970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
126300970526     C     SFLULT        IFEQ      0
126400970526     C                   EXSR      SETFIL
126500970526     C                   END
126600970526     C*
126700970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
126800970526     C                   Z-ADD     SFLULT        WFLULT
126900970526     C*
127000970526     C* CICLO:
127100970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
127200970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
127300970526     C                   Z-ADD     1             SFLRIG
127400970526     C     SFLRIG        DOWLE     SFLPAG
127500970526     C     SFLEOF        ANDEQ     0
127600970526     C*
127700970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
127800970526     C                   MOVEL     *BLANKS       OPZKEY
127900970526     C*
128000970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
128100970526     C                   EXSR      IRESFL
128200970526     C*
128300970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
128400970526     C                   ADD       1             SFLULT
128500970526     C*
128600970526     C* SCRITTURA RECORD SFL
128700160902     C*  imposto "effetti speciali"
128800160902     C                   EXSR      SFXD05
128900170201     C                   WRITE     LGOBD05
129000970526     C                   ADD       1             SFLRIG
129100970526     C*
129200970526     C* LETTURA FILE DA CARICARE
129300970526     C                   EXSR      LETFIL
129400970526     C                   END                                                    DO WHILE
129500970526     C*
129600970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
129700970526     C     WFLULT        IFNE      SFLULT
129800970526     C                   Z-ADD     WFLULT        SFLNBR
129900970526     C                   ADD       1             SFLNBR
130000970526     C                   END
130100970526     C*
130200970526     C                   ENDSR
130300970526     C*------------------------------------------------------------------------*
130400160902     C* SFXD05 - effetti speciali sfl D05 (RI, ND, BL, ...)
130500160902     C*          a seconda dei dati presenti in riga
130600970526     C*------------------------------------------------------------------------*
130700160902     C     SFXD05        BEGSR
130800161110     C*
130900160902     C*
131000160902     C                   ENDSR
131100160902     C*------------------------------------------------------------------------*
131200160902     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
131300160902     C*------------------------------------------------------------------------*
131400160902     C     OUTSFL        BEGSR
131500160902     C*
131600970526     C* WRITE RECORD TESTATA VIDEO
131700170201     C                   WRITE     LGOBD01
131800970526     C*
131900970526     C* VISUALIZZAZIONE MESSAGGIO
132000970526     C                   EXSR      SNDMSG
132100970526     C*
132200970526     C* WRITE RECORD OPZIONI VIDEO
132300170201     C                   WRITE     LGOBD02
132400970526     C*
132500970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
132600170201     C                   WRITE     LGOBD03
132700970526     C*
132800970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
132900170201     C                   WRITE     LGOBD04
133000970526     C*
133100970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
133200970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
133300970526     C     SFLEOF        IFEQ      0
133400970526     C                   MOVEL     '0'           *IN42
133500970526     C                   ELSE
133600970526     C*
133700970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
133800970526     C                   MOVEL     '1'           *IN42
133900970526     C                   END
134000970526     C*
134100970526     C* WRITE RECORD TASTI FUNZIONALI
134200170201     C                   WRITE     LGOBD09
134300970526     C*
134400970526     C* CASO DI SFL VUOTO
134500970526     C     SFLULT        IFLE      0
134600970526     C*
134700970526     C*    - WRITE RECORD VUOTO
134800170201     C                   WRITE     LGOBD0A
134900170201     C                   READ      LGOBD0A                                99
135000970526     C*
135100970526     C                   ELSE
135200970526     C*
135300970526     C* CASO DI SFL CON RECORDS
135400970526     C*
135500970526     C*    - WRITE RECORD CONTROLLO SFL
135600170201     C                   WRITE     LGOBD06
135700170201     C                   READ      LGOBD06                                99
135800170302     C* memo che c'è stata una exfmt del sfl
135900170302     C                   EVAL      wExfmt = *on
136000170329     C* tolgo memo di una richiesta
136100170329     C                   EVAL      wRichiesta = *blank
136200970526     C                   END
136300970526     C*
136400970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
136500170201     C                   READ      LGOBD03                                99
136600970526     C*
136700970526     C                   ENDSR
136800970526     C*------------------------------------------------------------------------*
136900970526     C* SNDMSG - MANDA MESSAGGIO
137000970526     C*------------------------------------------------------------------------*
137100970526     C     SNDMSG        BEGSR
137200970526     C*
137300970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
137400970526     C                   MOVEL     '1'           *IN40
137500170201     C                   WRITE     LGOBDMC
137600970526     C                   MOVEL     '0'           *IN40
137700970526     C*
137800970526     C* VISUALIZZAZIONE MESSAGGIO
137900970526     C                   Z-ADD     0             MSGULT
138000970526     C                   CALL      'XMSG'        XMSGPA
138100970526     C     DSMSER        IFEQ      0
138200970526     C                   MOVEL     DSMSKE        MSGKEY
138300970526     C                   END
138400970526     C                   Z-ADD     1             MSGULT
138500170201     C                   WRITE     LGOBDMS                                      SFL MESS
138600170201     C                   WRITE     LGOBDMC                                      CONTR MESS
138700970526     C*
138800970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
138900970526     C                   MOVEL     *BLANKS       DSMSMS
139000970526     C*
139100970526     C                   ENDSR
139200970526     C*------------------------------------------------------------------------*
139300970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
139400970526     C*------------------------------------------------------------------------*
139500970526     C     FUNCON        BEGSR
139600970526     C*
139700970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
139800970526     C                   EXSR      FUNVAR
139900970526     C*
140000970526     C* PULISCO CAMPI DI LAVORO
140100970526     C                   Z-ADD     0             I                 5 0
140200970526     C                   Z-ADD     0             W                 5 0
140300970526     C*
140400970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
140500970526     C     FUNKEY        IFNE      *BLANKS
140600970526     C     1             DO        24            I
140700970526     C                   MOVEL     FUD(I)        FUDRIG
140800970526     C*
140900970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
141000970526     C     FUNKEY        IFEQ      FUDFUN
141100970526     C     FUDATT        ANDEQ     'S'
141200970526     C                   Z-ADD     I             W
141300970526     C                   END
141400970526     C                   END                                                    DO WHILE
141500970526     C*
141600970526     C* FUNZIONE NON ATTIVA (ERRORE)
141700970526     C     W             IFEQ      0
141800970526     C*
141900970526     C* EMISSIONE MESSAGGIO DI ERRORE
142000970526     C                   MOVEL     MSG(2)        DSMSMS
142100970526     C                   END                                                    W=0
142200970526     C                   END                                                    FUNKEY NE *BLANKS
142300970526     C*
142400970526     C* FUNZIONE ATTIVA (PROCEDI ...)
142500970526     C     W             IFNE      0
142600970526     C     FUNKEY        OREQ      *BLANKS
142700970526     C*
142800970526     C     FUNKEY        IFEQ      *BLANKS
142900970526     C                   EXSR      ENTGES
143000970526     C                   END
143100970526     C     FUNKEY        IFEQ      '03'
143200970526     C                   EXSR      F03GES
143300970526     C                   END
143400970526     C     FUNKEY        IFEQ      '05'
143500970526     C                   EXSR      F05GES
143600970526     C                   END
143700970526     C     FUNKEY        IFEQ      '06'
143800970526     C                   EXSR      F06GES
143900970526     C                   END
144000161130     C     FUNKEY        IFEQ      '07'
144100161130     C                   EXSR      F07GES
144200161111     C                   END
144300161130     C     FUNKEY        IFEQ      '08'
144400161130     C                   EXSR      F08GES
144500161130     C                   END
144600160913     C     FUNKEY        IFEQ      '12'
144700160913     C                   EXSR      F12GES
144800160913     C                   END
144900170328     C     FUNKEY        IFEQ      '22'
145000170328     C                   EXSR      F22GES
145100970526     C                   END
145200170328     C     FUNKEY        IFEQ      '23'
145300170328     C                   EXSR      F23GES
145400170328     C                   END
145500970526     C     FUNKEY        IFEQ      '24'
145600970526     C                   EXSR      F24GES
145700970526     C                   END
145800970526     C     FUNKEY        IFEQ      'RU'
145900970526     C                   EXSR      RUPGES
146000970526     C                   END
146100970526     C*
146200970526     C                   END
146300970526     C*
146400970526     C                   ENDSR
146500970526     C*------------------------------------------------------------------------*
146600970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
146700970526     C*------------------------------------------------------------------------*
146800970526     C     FUNVAR        BEGSR
146900970526     C*
147000970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
147100970526     C     KEY           IFEQ      ENTER
147200970526     C                   MOVEL     *BLANKS       FUNKEY
147300970526     C                   END
147400970526     C     KEY           IFEQ      F1
147500970526     C                   MOVEL     '01'          FUNKEY
147600970526     C                   END
147700970526     C     KEY           IFEQ      F2
147800970526     C                   MOVEL     '02'          FUNKEY
147900970526     C                   END
148000970526     C     KEY           IFEQ      F3
148100970526     C                   MOVEL     '03'          FUNKEY
148200970526     C                   END
148300970526     C     KEY           IFEQ      F4
148400970526     C                   MOVEL     '04'          FUNKEY
148500970526     C                   END
148600970526     C     KEY           IFEQ      F5
148700970526     C                   MOVEL     '05'          FUNKEY
148800970526     C                   END
148900970526     C     KEY           IFEQ      F6
149000970526     C                   MOVEL     '06'          FUNKEY
149100970526     C                   END
149200970526     C     KEY           IFEQ      F7
149300970526     C                   MOVEL     '07'          FUNKEY
149400970526     C                   END
149500970526     C     KEY           IFEQ      F8
149600970526     C                   MOVEL     '08'          FUNKEY
149700970526     C                   END
149800970526     C     KEY           IFEQ      F9
149900970526     C                   MOVEL     '09'          FUNKEY
150000970526     C                   END
150100970526     C     KEY           IFEQ      F10
150200970526     C                   MOVEL     '10'          FUNKEY
150300970526     C                   END
150400970526     C     KEY           IFEQ      F11
150500970526     C                   MOVEL     '11'          FUNKEY
150600970526     C                   END
150700970526     C     KEY           IFEQ      F12
150800970526     C                   MOVEL     '12'          FUNKEY
150900970526     C                   END
151000970526     C     KEY           IFEQ      F13
151100970526     C                   MOVEL     '13'          FUNKEY
151200970526     C                   END
151300970526     C     KEY           IFEQ      F14
151400970526     C                   MOVEL     '14'          FUNKEY
151500970526     C                   END
151600970526     C     KEY           IFEQ      F15
151700970526     C                   MOVEL     '15'          FUNKEY
151800970526     C                   END
151900970526     C     KEY           IFEQ      F16
152000970526     C                   MOVEL     '16'          FUNKEY
152100970526     C                   END
152200970526     C     KEY           IFEQ      F17
152300970526     C                   MOVEL     '17'          FUNKEY
152400970526     C                   END
152500970526     C     KEY           IFEQ      F18
152600970526     C                   MOVEL     '18'          FUNKEY
152700970526     C                   END
152800970526     C     KEY           IFEQ      F19
152900970526     C                   MOVEL     '19'          FUNKEY
153000970526     C                   END
153100970526     C     KEY           IFEQ      F20
153200970526     C                   MOVEL     '20'          FUNKEY
153300970526     C                   END
153400970526     C     KEY           IFEQ      F21
153500970526     C                   MOVEL     '21'          FUNKEY
153600970526     C                   END
153700970526     C     KEY           IFEQ      F22
153800970526     C                   MOVEL     '22'          FUNKEY
153900970526     C                   END
154000970526     C     KEY           IFEQ      F23
154100970526     C                   MOVEL     '23'          FUNKEY
154200970526     C                   END
154300970526     C     KEY           IFEQ      F24
154400970526     C                   MOVEL     '24'          FUNKEY
154500970526     C                   END
154600970526     C     KEY           IFEQ      ROLLUP
154700970526     C                   MOVEL     'RU'          FUNKEY
154800970526     C                   END
154900970526     C     KEY           IFEQ      HELP
155000970526     C                   MOVEL     'HP'          FUNKEY
155100970526     C                   END
155200970526     C*
155300970526     C                   ENDSR
155400970526     C*------------------------------------------------------------------------*
155500970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
155600970526     C*------------------------------------------------------------------------*
155700970526     C     FUNGES        BEGSR
155800970526     C*
155900970526     C* PULISCO CAMPI DI LAVORO
156000970526     C                   Z-ADD     0             W                 5 0
156100970526     C*
156200970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
156300970526     C* E CARICO TABELLA FUNZIONI ATTIVE
156400970526     C     1             DO        24            I
156500970526     C                   MOVEL     FUD(I)        FUDRIG
156600970526     C*
156700970526     C* PULIZIA FUNZIONI PRECEDENTI
156800970526     C                   MOVEL     *BLANKS       FUV(I)
156900970526     C*
157000970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
157100970526     C* (SALTA F23 E F24)
157200970526     C     FUDATT        IFEQ      'S'
157300970526     C     FUDFUN        ANDNE     '23'
157400970526     C     FUDFUN        ANDNE     '24'
157500970526     C                   ADD       1             W
157600970526     C                   MOVEL     FUDDES        FUV(W)
157700970526     C                   END
157800970526     C                   END                                                    DO WHILE
157900970526     C*
158000970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
158100970526     C* ALTRIMENTI DISATTIVO F24
158200970526     C     1             DO        24            I
158300970526     C                   MOVEL     FUD(I)        FUDRIG
158400970526     C*
158500970526     C     FUDFUN        IFEQ      '24'
158600970526     C     W             IFGT      6
158700970526     C                   MOVEL     'S'           FUDATT
158800970526     C                   ELSE
158900970526     C                   MOVEL     *BLANKS       FUDATT
159000970526     C                   END                                                    W GT 8
159100970526     C                   MOVEL     FUDRIG        FUD(I)
159200970526     C                   END
159300970526     C                   END                                                    DO WHILE
159400970526     C*
159500970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
159600970526     C     W             DIV       6             I
159700970526     C                   MVR                     Z
159800970526     C*
159900970526     C     Z             IFNE      0
160000970526     C     I             ADD       1             Z
160100970526     C                   ELSE
160200970526     C                   Z-ADD     I             Z
160300970526     C                   END
160400970526     C*
160500970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
160600970526     C                   ADD       1             FUNGRU
160700970526     C     FUNGRU        IFGT      Z
160800970526     C     FUNGRU        ORLT      1
160900970526     C                   Z-ADD     1             FUNGRU
161000970526     C                   END
161100970526     C*
161200970526     C* PRIMO GRUPPO DI 6 FUNZIONI
161300970526     C     FUNGRU        IFEQ      1
161400970526     C                   MOVEL     FUV(1)        FUN001
161500970526     C                   MOVEL     FUV(2)        FUN002
161600970526     C                   MOVEL     FUV(3)        FUN003
161700970526     C                   MOVEL     FUV(4)        FUN004
161800970526     C                   MOVEL     FUV(5)        FUN005
161900970526     C                   MOVEL     FUV(6)        FUN006
162000970526     C                   END
162100970526     C*
162200970526     C* SECONDO GRUPPO DI 6 FUNZIONI
162300970526     C     FUNGRU        IFEQ      2
162400970526     C                   MOVEL     FUV(7)        FUN001
162500970526     C                   MOVEL     FUV(8)        FUN002
162600970526     C                   MOVEL     FUV(9)        FUN003
162700970526     C                   MOVEL     FUV(10)       FUN004
162800970526     C                   MOVEL     FUV(11)       FUN005
162900970526     C                   MOVEL     FUV(12)       FUN006
163000970526     C                   END
163100970526     C*
163200970526     C* TERZO GRUPPO DI 6 FUNZIONI
163300970526     C     FUNGRU        IFEQ      3
163400970526     C                   MOVEL     FUV(13)       FUN001
163500970526     C                   MOVEL     FUV(14)       FUN002
163600970526     C                   MOVEL     FUV(15)       FUN003
163700970526     C                   MOVEL     FUV(16)       FUN004
163800970526     C                   MOVEL     FUV(17)       FUN005
163900970526     C                   MOVEL     FUV(18)       FUN006
164000970526     C                   END
164100970526     C*
164200970526     C* QUARTO GRUPPO DI 6 FUNZIONI
164300970526     C     FUNGRU        IFEQ      4
164400970526     C                   MOVEL     FUV(19)       FUN001
164500970526     C                   MOVEL     FUV(20)       FUN002
164600970526     C                   MOVEL     FUV(21)       FUN003
164700970526     C                   MOVEL     FUV(22)       FUN004
164800970526     C                   MOVEL     *BLANKS       FUN005
164900970526     C                   MOVEL     *BLANKS       FUN006
165000970526     C                   END
165100970526     C*
165200970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
165300970526     C                   MOVEL     *BLANKS       FUN007
165400970526     C                   MOVEL     *BLANKS       FUN008
165500970526     C*
165600970526     C     1             DO        24            I
165700970526     C                   MOVEL     FUD(I)        FUDRIG
165800970526     C     FUDFUN        IFEQ      '23'
165900970526     C     FUDATT        ANDEQ     'S'
166000970526     C                   MOVEL     FUDDES        FUN007
166100970526     C                   END
166200970526     C     FUDFUN        IFEQ      '24'
166300970526     C     FUDATT        ANDEQ     'S'
166400970526     C                   MOVEL     FUDDES        FUN008
166500970526     C                   END
166600970526     C                   END                                                    DO
166700970526     C*
166800970526     C                   ENDSR
166900970526     C*------------------------------------------------------------------------*
167000970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
167100970526     C*------------------------------------------------------------------------*
167200970526     C     OPZGES        BEGSR
167300970526     C*
167400970526     C* PULISCO CAMPI DI LAVORO
167500970526     C                   Z-ADD     0             W                 5 0
167600970526     C*
167700970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
167800970526     C* E CARICO TABELLA OPZIONI ATTIVE
167900970526     C     1             DO        24            I
168000970526     C                   MOVEL     OPD(I)        OPDRIG
168100970526     C*
168200970526     C* PULIZIA OPZIONI PRECEDENTI
168300970526     C                   MOVEL     *BLANKS       OPV(I)
168400970526     C*
168500970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
168600970526     C     OPDATT        IFEQ      'S'
168700970526     C                   ADD       1             W
168800970526     C                   MOVEL     OPDDES        OPV(W)
168900970526     C                   END
169000970526     C                   END                                                    DO WHILE
169100970526     C*
169200970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
169300970526     C* ALTRIMENTI DISATTIVO F23
169400970526     C     1             DO        24            I
169500970526     C                   MOVEL     FUD(I)        FUDRIG
169600970526     C*
169700970526     C     FUDFUN        IFEQ      '23'
169800970526     C     W             IFGT      8
169900970526     C                   MOVEL     'S'           FUDATT
170000970526     C                   ELSE
170100970526     C                   MOVEL     *BLANKS       FUDATT
170200970526     C                   END                                                    W GT 8
170300970526     C                   MOVEL     FUDRIG        FUD(I)
170400970526     C                   END
170500970526     C                   END                                                    DO WHILE
170600970526     C*
170700970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
170800970526     C     W             DIV       8             I
170900970526     C                   MVR                     Z                 5 0
171000970526     C*
171100970526     C     Z             IFNE      0
171200970526     C     I             ADD       1             Z
171300970526     C                   ELSE
171400970526     C                   Z-ADD     I             Z
171500970526     C                   END
171600970526     C*
171700970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
171800970526     C                   ADD       1             OPZGRU
171900970526     C     OPZGRU        IFGT      Z
172000970526     C     OPZGRU        ORLT      1
172100970526     C                   Z-ADD     1             OPZGRU
172200970526     C                   END
172300970526     C*
172400970526     C* PRIMO GRUPPO DI 8 OPZIONI
172500970526     C     OPZGRU        IFEQ      1
172600970526     C                   MOVEL     OPV(1)        OPZ001
172700970526     C                   MOVEL     OPV(2)        OPZ002
172800970526     C                   MOVEL     OPV(3)        OPZ003
172900970526     C                   MOVEL     OPV(4)        OPZ004
173000970526     C                   MOVEL     OPV(5)        OPZ005
173100970526     C                   MOVEL     OPV(6)        OPZ006
173200970526     C                   MOVEL     OPV(7)        OPZ007
173300970526     C                   MOVEL     OPV(8)        OPZ008
173400970526     C                   END
173500970526     C*
173600970526     C* SECONDO GRUPPO DI 8 OPZIONI
173700970526     C     OPZGRU        IFEQ      2
173800970526     C                   MOVEL     OPV(9)        OPZ001
173900970526     C                   MOVEL     OPV(10)       OPZ002
174000970526     C                   MOVEL     OPV(11)       OPZ003
174100970526     C                   MOVEL     OPV(12)       OPZ004
174200970526     C                   MOVEL     OPV(13)       OPZ005
174300970526     C                   MOVEL     OPV(14)       OPZ006
174400970526     C                   MOVEL     OPV(15)       OPZ007
174500970526     C                   MOVEL     OPV(16)       OPZ008
174600970526     C                   END
174700970526     C*
174800970526     C* TERZO GRUPPO DI 8 OPZIONI
174900970526     C     OPZGRU        IFEQ      3
175000970526     C                   MOVEL     OPV(17)       OPZ001
175100970526     C                   MOVEL     OPV(18)       OPZ002
175200970526     C                   MOVEL     OPV(19)       OPZ003
175300970526     C                   MOVEL     OPV(20)       OPZ004
175400970526     C                   MOVEL     OPV(21)       OPZ005
175500970526     C                   MOVEL     OPV(22)       OPZ006
175600970526     C                   MOVEL     OPV(23)       OPZ007
175700970526     C                   MOVEL     OPV(24)       OPZ008
175800970526     C                   END
175900970526     C*
176000970526     C                   ENDSR
176100970526     C*------------------------------------------------------------------------*
176200970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
176300970526     C*------------------------------------------------------------------------*
176400970526     C     OPZCON        BEGSR
176500970526     C*
176600970526     C* PULISCO CAMPI DI LAVORO
176700970526     C                   Z-ADD     0             I                 5 0
176800970526     C                   Z-ADD     0             W                 5 0
176900970526     C*
177000970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
177100970526     C     1             DO        24            I
177200970526     C                   MOVEL     OPD(I)        OPDRIG
177300970526     C*
177400970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
177500970526     C     OPZKEY        IFEQ      OPDOPZ
177600970526     C     OPZKEY        OREQ      OPDOP1
177700970526     C*
177800970526     C     OPDATT        IFEQ      'S'
177900970526     C                   MOVEL     OPDOPZ        OPZKEY
178000970526     C                   Z-ADD     I             W
178100970526     C                   END
178200970526     C                   END
178300970526     C*
178400970526     C                   END                                                    DO WHILE
178500970526     C*
178600970526     C* OPZIONE NON ATTIVA (ERRORE)
178700970526     C     W             IFEQ      0
178800970526     C*
178900970526     C* OPZIONE NON OK
179000970526     C                   Z-ADD     1             OPZOK
179100970526     C*
179200970526     C* EMISSIONE MESSAGGIO DI ERRORE
179300970526     C                   MOVEL     MSG(1)        DSMSMS
179400970526     C                   ELSE
179500970526     C*
179600970526     C* OPZIONE ATTIVA (PROCEDI ...)
179700970526     C*
179800970526     C     OPZKEY        IFEQ      ' 1'
179900970526     C                   EXSR      O01GES
180000970526     C                   END
180100970526     C     OPZKEY        IFEQ      ' 2'
180200970526     C                   EXSR      O02GES
180300970526     C                   END
180400970603     C     OPZKEY        IFEQ      ' 3'
180500970603     C                   EXSR      O03GES
180600970603     C                   END
180700970526     C     OPZKEY        IFEQ      ' 4'
180800970526     C                   EXSR      O04GES
180900970526     C                   END
181000970526     C     OPZKEY        IFEQ      ' 5'
181100970526     C                   EXSR      O05GES
181200970526     C                   END
181300970526     C     OPZKEY        IFEQ      ' 7'
181400970526     C                   END
181500970528     C     OPZKEY        IFEQ      ' 8'
181600970528     C                   END
181700970528     C     OPZKEY        IFEQ      ' 9'
181800970528     C                   END
181900970528     C     OPZKEY        IFEQ      '10'
182000970528     C                   END
182100170327     C     OPZKEY        IFEQ      '11'
182200170327     C                   EXSR      O11GES
182300970528     C                   END
182400170504     C     OPZKEY        IFEQ      '12'
182500170504     C                   EXSR      O12GES
182600170504     C                   END
182700170327     C     OPZKEY        IFEQ      '14'
182800170327     C                   EXSR      O14GES
182900170327     C                   END
183000970526     C                   END                                                    OPZ.NON ATTIVA
183100970526     C*
183200970526     C                   ENDSR
183300160831
183400160831       /free
183500160831
183600160831       //--------------------------------------------------------------
183700160831       //?Preparazione stringa SQL                                     ?
183800160831       //--------------------------------------------------------------
183900160831       BEGSR  Prepara_SQL;
184000160831
184100160901         // Chiudo il cursore (lo faccio in anticipo perché non saprei quando chiuderlo
184200160901         exec sql  close C1;
184300160901
184400161130         select;
184500161130
184600170202          // tengo la gestione drill anche se PER ORA non è usata
184700170202          when wLivDrill = 1;
184800170202           wSQL = 'select foblogica, fobdtestr, foblib, fobobjt, fobobj, +
184900170505                   fobattr, fobobjd, fobdati +
185000170202                   from wafob00f';
185100170117           // costruisce la where
185200170117           exsr CreateWhere;
185300161111           wSQL = %trim(wSQL) +
185400170202                  ' order by foblogica, foblib, fobobjt, fobobj';
185500161130
185600161130         endsl;
185700160831
185800160831         // Dichiarazione cursore
185900160831         exec sql   prepare S1   from :wSQL;
186000160831         exec sql   declare C1  asensitive   cursor for S1;
186100160831
186200160831         // Apertura del cursore
186300160831         exec sql   open C1;
186400160831
186500160901         *in99 = *off;
186600160901         if SQLCode < 0;
186700160901           *in99 = *on;
186800160831         endif;
186900160831
187000160831       ENDSR;
187100160831
187200160831       //--------------------------------------------------------------
187300170117       //?Costruisce la where a seconda delle selezioni                ?
187400160831       //--------------------------------------------------------------
187500170117       BEGSR  CreateWhere;
187600160831
187700170117         // se è stato indicato almeno una selezione, allora avrò una where
187800170201         if D03Logica <> *blank
187900170201         or D03Lib    <> *blank
188000170201         or D03ObjT   <> *blank
188100170201         or D03Obj    <> *blank
188200170201         or D03Attr   <> *blank
188300170306         or D03UtC    <> *blank
188400170306         or D03Dati   <> *blank;
188500170117           wSQL = %trim(wSQL) +
188600170117                  ' where';
188700170201           if D03Logica <> *blank;
188800170117           wSQL = %trim(wSQL) +
188900170201                  ' FOBLogica = ''' +
189000170201                  D03Logica +
189100170118                  '''';
189200170117           endif;
189300170201           if D03Lib <> *blank;
189400170202             if D03Logica <> *blank;
189500170118               wSQL = %trim(wSQL) +
189600170118               ' and';
189700170118             endif;
189800170201             wSQL = %trim(wSQL) +
189900170201                  ' FOBLib like ''%' +
190000170202                  %trim(D03Lib) +
190100170201                  '%''';
190200170118           endif;
190300170201           if D03ObjT <> *blank;
190400170202             if D03Logica <> *blank
190500170201             or D03Lib <> *blank;
190600170118               wSQL = %trim(wSQL) +
190700170118               ' and';
190800170118             endif;
190900170201             wSQL = %trim(wSQL) +
191000170201                  ' FOBObjT like ''%' +
191100170202                  %trim(D03ObjT) +
191200170201                  '%''';
191300170118           endif;
191400170201           if D03Obj <> *blank;
191500170202             if D03Logica <> *blank
191600170201             or D03Lib <> *blank
191700170201             or D03ObjT <> *blank;
191800170201               wSQL = %trim(wSQL) +
191900170201               ' and';
192000170201             endif;
192100170201             wSQL = %trim(wSQL) +
192200170201                  ' FOBObj like ''%' +
192300170202                  %trim(D03Obj) +
192400170201                  '%''';
192500170201           endif;
192600170201           if D03Attr <> *blank;
192700170202             if D03Logica <> *blank
192800170201             or D03Lib <> *blank
192900170201             or D03ObjT <> *blank
193000170201             or D03Obj <> *blank;
193100170201               wSQL = %trim(wSQL) +
193200170201               ' and';
193300170201             endif;
193400170201             wSQL = %trim(wSQL) +
193500170201                  ' FOBAttr like ''%' +
193600170202                  %trim(D03Attr) +
193700170201                  '%''';
193800170201           endif;
193900170306           if D03UtC <> *blank;
194000170202             if D03Logica <> *blank
194100170201             or D03Lib <> *blank
194200170201             or D03ObjT <> *blank
194300170201             or D03Obj <> *blank
194400170201             or D03Attr <> *blank;
194500170201               wSQL = %trim(wSQL) +
194600170201               ' and';
194700170201             endif;
194800170201             wSQL = %trim(wSQL) +
194900170306                  ' substr(FOBDati, 1 , 10) like ''%' +
195000170306                  %trim(D03UtC) +
195100170201                  '%''';
195200170201           endif;
195300170306           if D03Dati <> *blank;
195400170306             if D03Logica <> *blank
195500170306             or D03Lib <> *blank
195600170306             or D03ObjT <> *blank
195700170306             or D03Obj <> *blank
195800170306             or D03Attr <> *blank
195900170306             or D03UtC  <> *blank;
196000170306               wSQL = %trim(wSQL) +
196100170306               ' and';
196200170306             endif;
196300170306             wSQL = %trim(wSQL) +
196400170306                  ' FOBDati like ''%' +
196500170306                  %trim(D03Dati) +
196600170306                  '%''';
196700170306           endif;
196800170117         endif;
196900170117
197000170117       ENDSR;
197100170117
197200170117       //--------------------------------------------------------------
197300170117       //?Lettura occorrenza                                           ?
197400170117       //--------------------------------------------------------------
197500170117       BEGSR  Legge_SQL;
197600170117
197700161130         select;
197800161130
197900170201          // liv.1
198000170201          when wLivDrill = 1;
198100170201           exec sql  fetch next  from C1  into :WAFOB_fetch1;
198200161130         endsl;
198300160901
198400160901         *in99 = *off;
198500160901         if SQLCode = 100 or SQLCode < 0;
198600160901           *in99 = *on;
198700160901         endif;
198800160831
198900160831       ENDSR;
199000160831
199100170323       //--------------------------------------------------------------
199200170323       // Esecuzione del comando (già impostato).                      ?
199300170323       //--------------------------------------------------------------
199400170323       begsr  sr_ExecCmd;
199500170323
199600170323         clear Qcap0100;
199700170323         Qcabcsdh = *off;
199800170323         Qcapa    = *off;
199900170504         Qcacmdss = *off;
200000170323         Qcaerved = *allX'00';
200100170323
200200170323         clear Qusec;
200300170323         Qusbprv  = %size(Qusec);
200400170323
200500170323         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
200600170323                           %size(Qcap0100) : 'CPOP0100' : *omit :
200700170323                           0 : 0 : Qusec);
200800170323
200900170323         //if  Qusei <> *blank;
201000170323         //  ...;
201100170323         //endif;
201200170323
201300170323       endsr;
201400160831
201500970526     C*------------------------------------------------------------------------*
201600970526     C* NOTE COSTRUZIONE SCHIERE
201700970526     C*------------------------------------------------------------------------*
201800970526     C*
201900970526     C* -MSG-MESSAGGI
202000970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
202100970526     C* (NON TOCCARE MSG,1 E MSG,2)
202200970526     C*
202300970526     C* -CMD-COMANDI
202400970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
202500970526     C*
202600970526     C* -OPD-OPZIONI
202700970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
202800970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
202900970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
203000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
203100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
203200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
203300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
203400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
203500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
203600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
203700970526     C*
203800970526     C* -FUD-FUNZIONI
203900970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
204000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
204100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
204200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
204300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
204400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
204500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
204600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
204700970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
204800970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
204900970526     O*------------------------------------------------------------------------*
205000970526** DEC - DECODIFICHE
205100970526    GESTIONE                                                     1
205200970526INTERROGAZIONE                                                   2
205300970530     SCELTA                                                      3
2054009705264
2055009705265
205600970526** MSG - MESSAGGI
205700170201UBLGOBAR- Opzione non gestita                                                  1
205800170201UBLGOBAR- Tasto funzionale non gestito                                         2
205900170327UBLGOBAR- Cmd non eseguito:                                                    3
206000170201UBLGOBAR- Record annullato.                                                    4
206100170201UBLGOBAR- Record non annullato. Impossibile ripristinare                       5
206200170201UBLGOBAR- Accettato solo un parametro di inizio lista                          6
206300170201UBLGOBAR- Errori nei parametri di ingresso                                     7
206400170201UBLGOBAR- Data errata                                                          8
206500170201UBLGOBAR- Valore DA > valore AL                                                9
206600170201UBLGOBAR- Errore SQL nel reperimento dati Validi al                           10
206700170201UBLGOBAR- Copia totale (opz. 23) eseguita correttamente                       11
206800170201UBLGOBAR- Copia totale (opz. 23) non eseguita                                 12
206900170201UBLGOBAR- Non esiste un livello superiore per il DrillUp                      13
207000170201UBLGOBAR- Non esiste un livello inferiore per il DrillDown                    14
207100170327UBLGOBAR- ELABORAZIONE ESEGUITA                                               15
207200170328UBLGOBAR- STAMPA ESEGUITA                                                     16
207300170201UBLGOBAR- 17                                                                  17
207400170201UBLGOBAR- 18                                                                  18
207500170201UBLGOBAR- 19                                                                  19
207600170201UBLGOBAR- 20                                                                  20
207700970526** CMD - COMANDI CL
207800161108
207900970526
208000970526
208100970526
208200970526
208300970526** OPD - OPZIONI
208400170323 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
208500170202 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
208600170327 3=Copia           -3 -     (S/N)=N                        opz. 03¬
208700170504 4=Elimina         -4 -     (S/N)=N                        opz. 04¬
208800170504 5=Visualizza      -5 -     (S/N)=S                        opz. 05¬
208900160713                   -  -     (S/N)=N                        opz. 06¬
209000160713                   -  -     (S/N)=N                        opz. 07¬
209100160713                   -  -     (S/N)=N                        opz. 08¬
209200160713                   -  -     (S/N)=N                        opz. 09¬
209300160713                   -  -     (S/N)=N                        opz. 10¬
20940017050411=Muove in Replace-11-     (S/N)=N                        opz. 11¬
20950017050412=Allinea SETRAS  -12-     (S/N)=N                        opz. 12¬
209600161108                   -  -     (S/N)=N                        opz. 13¬
20970017030214=Storicizzazione -14-     (S/N)=N                        opz. 14¬
209800970526                   -  -     (S/N)=N                        opz. 15¬
209900970526                   -  -     (S/N)=N                        opz. 16¬
210000970526                   -  -     (S/N)=N                        opz. 17¬
210100970526                   -  -     (S/N)=N                        opz. 18¬
210200970526                   -  -     (S/N)=N                        opz. 19¬
210300970526                   -  -     (S/N)=N                        opz. 20¬
210400970526                   -  -     (S/N)=N                        opz. 21¬
210500970526                   -  -     (S/N)=N                        opz. 22¬
210600161108                   -  -     (S/N)=N                        opz. 23¬
210700970526                   -  -     (S/N)=N                        opz. 24¬
210800970526** FUD - FUNZIONI
210900170407                   -        (S/N)=N                        funz.01¬
211000970526                   -        (S/N)=N                        funz.02¬
211100970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
211200970526                   -        (S/N)=N                        funz.04¬
211300970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
211400970526F06=Inserimento    -        (S/N)=N                        funz.06¬
211500170202F07=Drill-down     -        (S/N)=N                        funz.07¬
211600170202F08=Drill-up       -        (S/N)=N                        funz.08¬
211700170202                   -        (S/N)=N                        funz.09¬
211800160713                   -        (S/N)=N                        funz.10¬
211900160713                   -        (S/N)=N                        funz.11¬
212000160923F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
212100160713                   -        (S/N)=N                        funz.13¬
212200970603                   -        (S/N)=S                        funz.14¬
212300970526                   -        (S/N)=N                        funz.15¬
212400970526                   -        (S/N)=N                        funz.16¬
212500970526                   -        (S/N)=N                        funz.17¬
212600970526                   -        (S/N)=N                        funz.18¬
212700970627 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
212800970627 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
212900970526                   -        (S/N)=N                        funz.21¬
213000170328F22=Stampa selez.  -        (S/N)=S                        funz.22¬
213100970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
213200970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
