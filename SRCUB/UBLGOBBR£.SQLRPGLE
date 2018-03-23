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
011800161111
011900160831       // - Stringa SQL da eseguire
012000160831     d wSQl            s           1024    inz  varying
012100170201     d WAFOB_fetch1    ds                  qualified
012200170201     d  Logica                        2a
012300170202     d  DtEstr                         d
012400170201     d  Lib                          10a
012500170201     d  ObjT                          7a
012600170201     d  Obj                          10a
012700170201     d  Attr                         10a
012800170201     d  Dati                        130a
012900170323
013000170323       // -?Tasti funzionali a video?
013100170323     d c_F01           c                   const(x'31')
013200170323     d c_F02           c                   const(x'32')
013300170323     d c_F03           c                   const(x'33')
013400170323     d c_F04           c                   const(x'34')
013500170323     d c_F05           c                   const(x'35')
013600170323     d c_F06           c                   const(x'36')
013700170323     d c_F07           c                   const(x'37')
013800170323     d c_F08           c                   const(x'38')
013900170323     d c_F09           c                   const(x'39')
014000170323     d c_F10           c                   const(x'3A')
014100170323     d c_F11           c                   const(x'3B')
014200170323     d c_F12           c                   const(x'3C')
014300170323     d c_F13           c                   const(x'B1')
014400170323     d c_F14           c                   const(x'B2')
014500170323     d c_F15           c                   const(x'B3')
014600170323     d c_F16           c                   const(x'B4')
014700170323     d c_F17           c                   const(x'B5')
014800170323     d c_F18           c                   const(x'B6')
014900170323     d c_F19           c                   const(x'B7')
015000170323     d c_F20           c                   const(x'B8')
015100170323     d c_F21           c                   const(x'B9')
015200170323     d c_F22           c                   const(x'BA')
015300170323     d c_F23           c                   const(x'BB')
015400170323     d c_F24           c                   const(x'BC')
015500170323     d c_Enter         c                   const(x'F1')
015600170323     d c_RollDown      c                   const(x'F4')
015700170323     d c_RollUp        c                   const(x'F5')
015800170323
015900170323       // - Parametri gestione errori API
016000170323      /copy qSysInc/qRpgleSrc,QUSEC
016100170323
016200170323       // Parametri API QCAPCMD (Process Commands)?
016300170323     d Qcmd            s           1024    inz  varying
016400170323      /copy qSysInc/qRpgleSrc,QCAPCMD
016500170323      /copy gaitrasrc/srcProtoPR,QCAPCMD
016600161110
016700970526     C*------------------------------------------------------------------------*
016800970526     C* MAIN LINES
016900970526     C*------------------------------------------------------------------------*
017000970526     C*
017100970526     C* SE NON CI SONO ERRORI, PROSEGUE
017200970526IF  1C     ERRL00        IFNE      '1'
017300970526     C*
017400970526     C* CARICA IL SFILE
017500970526     C                   EXSR      POSSFL
017600970526     C*
017700970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
017800970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
017900970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
018000970526     C     FINL00        OREQ      '1'                                          *FINE PGM
018100970526     C*
018200970526     C* REIMPOSTA ERRORE DI PROCERURA
018300970526     C                   MOVEL     '0'           ERRL00
018400970526     C*
018500970526     C* EMETTE IL SFILE
018600970526     C                   EXSR      OUTSFL
018700970526     C*
018800970526     C* GESTIONE TASTI FUNZIONALI
018900970526     C                   EXSR      FUNCON
019000970526     C*
019100970526E   2C                   ENDDO
019200970526E   1C                   ENDIF
019300970526     C* FINE PGM
019400970526     C                   SETON                                        LR
019500970526     C*------------------------------------------------------------------------*
019600970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
019700970526     C*------------------------------------------------------------------------*
019800970526     C     F03GES        BEGSR
019900970526     C*
020000970526     C* IMPOSTA "PREMUTO F03"
020100970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
020200170202     C                   MOVEL     '1'           GOBAF03                         *DI PROCEDURA
020300160916     C*
020400970526     C                   ENDSR
020500970526     C*------------------------------------------------------------------------*
020600970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
020700970526     C*------------------------------------------------------------------------*
020800970526     C     F05GES        BEGSR
020900160923     C*
021000970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
021100970526     C*
021200970526     C                   ENDSR
021300970526     C*------------------------------------------------------------------------*
021400970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
021500970526     C*------------------------------------------------------------------------*
021600970526     C     F06GES        BEGSR
021700970526     C*
021800970526     C* INSERIMENTO CONSENTITO SOLO IN SEDE
021900020919IF  1C     DUTLPO        IFNE      'S'
022000970526     C                   MOVEL     '1'           ERRL00
022100170323     C                   MOVEL     MSG(20)       DSMSMS
022200970526E   1C                   ENDIF
022300970526     C*
022400970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
022500161108     C***                CLEAR                   DPROC
022600970529     C                   MOVEL     DEPOP0        WOP0
022700970529     C                   MOVEL     'F06'         WOP1
022800170323     C                   MOVEL     'XXXXXXXXX'   WPGM             10
022900970526     C                   EXSR      LANPGM                                       *LANCIA PGM
023000970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
023100970526E   1C                   ENDIF
023200970526     C*
023300970526     C                   ENDSR
023400161130     C*------------------------------------------------------------------------*
023500161201     C* F08GES - GESTIONE F08 -> Drill-up
023600161130     C*------------------------------------------------------------------------*
023700161201     C     F08GES        BEGSR
023800161130     C*
023900161130     C                   if        wLivDrill= 1
024000161130     C                   MOVEL     '1'           ERRL00
024100161201     C                   MOVEL     MSG(13)       DSMSMS
024200161130     C                   LEAVESR
024300161130     C                   endif
024400161130
024500161130     C* diminuisce il livello di Drill
024600161130     C                   eval      wLivDrill -= 1
024700161130     C*
024800161130     c                   eval      FUNGru = 0
024900161130     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
025000161130     C*
025100161130     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
025200161130     C                   EXSR      NEWPOS
025300161130     C*
025400161130     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
025500161130IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
025600161130IF  2C     SWPOSI        IFNE      0
025700161130     C                   EXSR      POSSFL                                       *RICARICA SFILE
025800161130X   2C                   ELSE
025900161130IF  3C     SFLULT        IFNE      0
026000161130     C                   EXSR      ENTATT                                       *GESTIONE ENTER
026100161130     C*
026200161130     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
026300161130IF  4C     SWMODI        IFNE      0
026400161130     C                   EXSR      POSSFL
026500161130E   4C                   ENDIF
026600161130E   3C                   ENDIF
026700161130E   2C                   ENDIF
026800161130E   1C                   ENDIF
026900161130     C*
027000161130     C                   ENDSR
027100161111     C*------------------------------------------------------------------------*
027200161201     C* F07GES - GESTIONE F07 -> Drill-down
027300161111     C*------------------------------------------------------------------------*
027400161201     C     F07GES        BEGSR
027500161111     C*
027600170201     C                   if        wLivDrill= 99
027700161130     C                   MOVEL     '1'           ERRL00
027800161201     C                   MOVEL     MSG(14)       DSMSMS
027900161130     C                   LEAVESR
028000161130     C                   endif
028100161130
028200161130     C* aumento il livello di Drill
028300161130     C                   eval      wLivDrill += 1
028400161111     C*
028500161111     c                   eval      FUNGru = 0
028600161111     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
028700161111     C*
028800161111     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
028900161111     C                   EXSR      NEWPOS
029000161111     C*
029100161111     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
029200161111IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
029300161111IF  2C     SWPOSI        IFNE      0
029400161111     C                   EXSR      POSSFL                                       *RICARICA SFILE
029500161111X   2C                   ELSE
029600161111IF  3C     SFLULT        IFNE      0
029700161111     C                   EXSR      ENTATT                                       *GESTIONE ENTER
029800161111     C*
029900161111     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
030000161111IF  4C     SWMODI        IFNE      0
030100161111     C                   EXSR      POSSFL
030200161111E   4C                   ENDIF
030300161111E   3C                   ENDIF
030400161111E   2C                   ENDIF
030500161111E   1C                   ENDIF
030600161111     C*
030700161111     C                   ENDSR
030800970526     C*------------------------------------------------------------------------*
030900970526     C* F12GES - GESTIONE F12 -> RITORNO
031000970526     C*------------------------------------------------------------------------*
031100970526     C     F12GES        BEGSR
031200970526     C*
031300970526     C* IMPOSTA "PREMUTO F12"
031400970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
031500170202     C                   MOVEL     '1'           GOBAF12                         *DI PROCEDURA
031600970526     C*
031700970526     C                   ENDSR
031800970526     C*------------------------------------------------------------------------*
031900970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
032000970526     C*------------------------------------------------------------------------*
032100970526     C     F23GES        BEGSR
032200970526     C*
032300970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
032400970526     C*
032500970526     C                   ENDSR
032600970526     C*------------------------------------------------------------------------*
032700970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
032800970526     C*------------------------------------------------------------------------*
032900970526     C     F24GES        BEGSR
033000970526     C*
033100970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
033200970526     C*
033300970526     C                   ENDSR
033400970526     C*------------------------------------------------------------------------*
033500970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
033600970526     C*------------------------------------------------------------------------*
033700970526     C     RUPGES        BEGSR
033800970526     C*
033900970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
034000970526     C*
034100970526     C                   ENDSR
034200970526     C*------------------------------------------------------------------------*
034300970526     C* ENTGES - GESTIONE ENTER -> INVIO
034400970526     C*------------------------------------------------------------------------*
034500970526     C     ENTGES        BEGSR
034600970526     C*
034700970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
034800970526     C                   EXSR      NEWPOS
034900970526     C*
035000970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
035100970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
035200970526IF  2C     SWPOSI        IFNE      0
035300970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
035400970526X   2C                   ELSE
035500970526IF  3C     SFLULT        IFNE      0
035600970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
035700970526     C*
035800970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
035900970526IF  4C     SWMODI        IFNE      0
036000970526     C                   EXSR      POSSFL
036100970526E   4C                   ENDIF
036200970526E   3C                   ENDIF
036300970526E   2C                   ENDIF
036400970526E   1C                   ENDIF
036500970526     C*
036600970526     C                   ENDSR
036700970526     C*------------------------------------------------------------------------*
036800970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
036900970526     C*------------------------------------------------------------------------*
037000970526     C     O01GES        BEGSR
037100970526     C*
037200160923     C*
037300160923     C* simulo l'enter
037400160923     C                   exsr      NEWPOS
037500160923     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
037600160923     C*
037700970526     C                   ENDSR
037800970526     C*------------------------------------------------------------------------*
037900970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
038000970526     C*------------------------------------------------------------------------*
038100970526     C     O02GES        BEGSR
038200160927     C*
038300160927     C*
038400161108     C                   ENDSR
038500970526     C*------------------------------------------------------------------------*
038600161108     C* O03GES - GESTIONE OPZIONE 3=COPIA
038700970526     C*------------------------------------------------------------------------*
038800161108     C     O03GES        BEGSR
038900160927     C*
039000170327
039100170327         // Emissione window?di richiesta libreria di arrivo
039200170327         *in45 = *on;
039300170327         *in46 = *off;
039400170327         *in47 = *on;
039500170327         clear LGOBW01;
039600170327         clear W01txt2 ;
039700170327         clear W01txt3 ;
039800170327         W01txt1 = 'In che libreria copio l''oggetto?';
039900170327         dou Key = c_F06
040000170327          or Key = c_F12;
040100170327         exfmt LGOBW01;
040200170327
040300170327           if Key = c_F06;
040400170327             QCmd =
040500170327              'CRTDUPOBJ OBJ(' + %trim(D05Obj) + ')';
040600170327             QCmd = %trim(QCmd) +
040700170327              ' FROMLIB(' + %trim(D05Lib) ;
040800170327             QCmd = %trim(QCmd) + ') OBJTYPE(' + %trim(D05ObjT);
040900170327             QCmd = %trim(QCmd) + ') TOLIB(' + %trim(W01Txt2) + ')';
041000170327
041100170327             // copio oggetto sul sistema
041200170327             exsr sr_execCmd;
041300170327             if  Qusei <> *blank;
041400170327               OpzOK = 1;
041500170327               // EMISSIONE MESSAGGIO DI ERRORE
041600170327               DSMSMS = %trim(MSG(3)) + ' ' + %trim(QCmd);
041700170327             else;
041800170327               // EMISSIONE MESSAGGIO DI AVVERTIMENTO
041900170327               DSMSMS = %trim(MSG(15));
042000170327             endif;
042100170327           endif;
042200170327
042300170327         enddo;
042400970526     C*
042500970526     C                   ENDSR
042600161108     C*------------------------------------------------------------------------*
042700161108     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
042800161108     C*------------------------------------------------------------------------*
042900161108     C     O04GES        BEGSR
043000161108     C*
043100170323
043200170323         // Emissione window?di richiesta conferma
043300170327         *in45 = *on;
043400170327         *in46 = *on;
043500170327         *in47 = *on;
043600170323         clear LGOBW01;
043700170323         clear W01txt1 ;
043800170323         clear W01txt3 ;
043900170323         W01txt2 = 'Confermi la cancellazione dell''oggetto?';
044000170323         dou Key = c_F06
044100170323          or Key = c_F12;
044200170323         exfmt LGOBW01;
044300170323
044400170323           if Key = c_F06;
044500170323             QCmd =
044600170323              'DLTOBJ OBJ(' + %trim(D05Lib) ;
044700170323             QCmd = %trim(QCmd) + '/' + %trim(D05Obj);
044800170323             QCmd = %trim(QCmd) + ') OBJTYPE(' + %trim(D05ObjT);
044900170323             QCmd = %trim(QCmd) + ')';
045000170323
045100170323             // cancello oggetto sul sistema
045200170323             exsr sr_execCmd;
045300170323             if  Qusei <> *blank;
045400170323               OpzOK = 1;
045500170323               // EMISSIONE MESSAGGIO DI ERRORE
045600170323               DSMSMS = %trim(MSG(3)) + ' ' + %trim(QCmd);
045700170323             else;
045800170323
045900170323               // cancello il rcd su WAFOB
046000170323               QCmd =
046100170323                'delete from WAFOB00F +
046200170323                 where +
046300170323                 FOBDTESTR = ''' + %char(%date(D05DtEstr:*eur)) + ''' and +
046400170323                 FOBLOGICA = ''' + %trim(D05Logica) + ''' and +
046500170323                 FOBOBJ    = ''' + %trim(D05Obj) + ''' and +
046600170323                 FOBLIB    = ''' + %trim(D05Lib) + ''' and +
046700170323                 FOBOBJT   = ''' + %trim(D05ObjT) + '''';
046800170323
046900170323               exec sql execute immediate :QCmd;
047000170323               if SQLCod < 0 or SQLCod = 100;
047100170323                 OpzOK = 1;
047200170323                 // EMISSIONE MESSAGGIO DI ERRORE
047300170323                 DSMSMS = %trim(MSG(3)) + ' ' + %trim(QCmd);
047400170323               endif;
047500170323
047600170323             endif;
047700170323           endif;
047800170323
047900170323         enddo;
048000161108     C*
048100161108     C                   ENDSR
048200970526     C*------------------------------------------------------------------------*
048300970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
048400970526     C*------------------------------------------------------------------------*
048500970526     C     O05GES        BEGSR
048600970526     C*
048700170301     C                   MOVEL     DEPOP0        WOP0
048800170301     C                   MOVEL     'O05'         WOP1
048900170301     C                   select
049000170301     C                   when      D05Logica = '01'
049100170301     C                   EVAL      BA01Logica     = D05Logica
049200170301     C                   EVAL      BA01Obj        = D05Obj
049300170301     C                   EVAL      BA01Lib        = D05Lib
049400170301     C                   EVAL      BA01ObjT       = D05ObjT
049500170307     C                   EVAL      BA01DTESTR     = D05DTESTRI
049600170301     C                   MOVEL     'UBLGOBA01R'  WPGM
049700170323     C*
049800170323     C                   when      D05Logica = '02'
049900170323     C                   EVAL      BA02Logica     = D05Logica
050000170323     C                   EVAL      BA02Obj        = D05Obj
050100170323     C                   EVAL      BA02Lib        = D05Lib
050200170323     C                   EVAL      BA02ObjT       = D05ObjT
050300170323     C                   EVAL      BA02DTESTR     = D05DTESTRI
050400170323     C                   MOVEL     'UBLGOBA02R'  WPGM
050500170301     C                   endsl
050600170301     C                   EXSR      LANPGM
050700970526     C*
050800970526     C                   ENDSR
050900170327     C*------------------------------------------------------------------------*
051000170327     C* O11GES - GESTIONE OPZIONE 11=MUOVI
051100170327     C*------------------------------------------------------------------------*
051200170327     C     O11GES        BEGSR
051300170327     C*
051400170327
051500170327         // Emissione window?di richiesta libreria di arrivo
051600170327         *in45 = *on;
051700170327         *in46 = *off;
051800170327         *in47 = *on;
051900170327         clear LGOBW01;
052000170327         clear W01txt2 ;
052100170327         clear W01txt3 ;
052200170327         W01txt1 = 'In che libreria sposto l''oggetto?';
052300170327         dou Key = c_F06
052400170327          or Key = c_F12;
052500170327         exfmt LGOBW01;
052600170327
052700170327           if Key = c_F06;
052800170327             QCmd =
052900170327              'MOVOBJ OBJ(' + %trim(D05Lib) ;
053000170327             QCmd = %trim(QCmd) + '/' + %trim(D05Obj);
053100170327             QCmd = %trim(QCmd) + ') OBJTYPE(' + %trim(D05ObjT);
053200170327             QCmd = %trim(QCmd) + ') TOLIB(' + %trim(W01Txt2) + ')';
053300170327
053400170327             // sposto oggetto sul sistema
053500170327             exsr sr_execCmd;
053600170327             if  Qusei <> *blank;
053700170327               OpzOK = 1;
053800170327               // EMISSIONE MESSAGGIO DI ERRORE
053900170327               DSMSMS = %trim(MSG(3)) + ' ' + %trim(QCmd);
054000170327             else;
054100170327               // EMISSIONE MESSAGGIO DI AVVERTIMENTO
054200170327               DSMSMS = %trim(MSG(15)) +
054300170327                ' - Dati non più veritieri, aggiornarli';
054400170327             endif;
054500170327           endif;
054600170327
054700170327         enddo;
054800170327     C*
054900170327     C                   ENDSR
055000970526     C*------------------------------------------------------------------------*
055100170302     C* O14GES - Storicizzazione
055200970526     C*------------------------------------------------------------------------*
055300170302     C     O14GES        BEGSR
055400970526     C*
055500170307     C                   CALL      'UBLGOBSTR'
055600170307     C                   PARM                    D05DtEstrI
055700170307     C                   PARM                    D05Logica
055800170307     C                   PARM                    D05Obj
055900170302     C                   PARM                    D05Lib
056000170302     C                   PARM                    D05ObjT
056100170303     C                   PARM                    D05Dati
056200170302     C                   PARM                    wExfmt
056300170302     C*
056400170302     C* tolgo memo che c'è stata una exfmt del sfl
056500170302     C                   EVAL      wExfmt = *off
056600970526     C*
056700970526     C                   ENDSR
056800970526     C*------------------------------------------------------------------------*
056900970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
057000970526     C*------------------------------------------------------------------------*
057100970526     C     NEWPOS        BEGSR
057200970526     C*
057300970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
057400970526     C*         1= RICHIESTO POSIZIONAMENTO
057500970526     C                   Z-ADD     0             SWPOSI            7 0
057600970526     C*
057700970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
057800970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
057900970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
058000970526     C                   Z-ADD     0             SWMODI            1 0
058100970526     C*
058200970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
058300161130     C     wLivDrill     IFNE      DEPLivDrill
058400170201     C     D03Logica     ORNE      DEPLogica
058500170201     C     D03Lib        ORNE      DEPLib
058600170201     C     D03ObjT       ORNE      DEPObjT
058700170201     C     D03Obj        ORNE      DEPObj
058800170201     C     D03Attr       ORNE      DEPAttr
058900170306     C     D03UtC        ORNE      DEPUtC
059000170306     C     D03Dati       ORNE      DEPDati
059100970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
059200970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
059300970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
059400161130     C                   EVAL      DEPLivDrill = wLivDrill
059500170202     C                   EVAL      DEPLogica = D03Logica
059600170202     C                   EVAL      DEPLib    = D03Lib
059700170202     C                   EVAL      DEPObjT   = D03ObjT
059800170202     C                   EVAL      DEPObj    = D03Obj
059900170202     C                   EVAL      DEPAttr   = D03Attr
060000170306     C                   EVAL      DEPUtC    = D03UtC
060100170306     C                   EVAL      DEPDati   = D03Dati
060200970526E   2C                   ENDIF
060300970526E   1C                   ENDIF
060400970526     C*
060500970526     C                   ENDSR
060600970526     C*------------------------------------------------------------------------*
060700970526     C* SETFIL - POSIZIONAMENTO RECORD
060800970526     C*------------------------------------------------------------------------*
060900970526     C     SETFIL        BEGSR
061000970526     C*
061100970526     C* POSIZIONAMENTO DEL FILE E PRIMA LETTURA IN BASE AL TIPO DI RICERCA
061200160830     C                   SELECT
061300160831     C*
061400161108IF  1C     WTIPRI        WHENEQ    *blank
061500160831     C* i dati li reperisce mediante SQL
061600160831     C                   EXSR      Prepara_SQL
061700160830     C*
061800160830E   1C                   ENDSL
061900970526     C* LETTURA RECORD
062000970526IF  1C     *IN99         IFEQ      *OFF
062100970526     C                   EXSR      LETFIL
062200970526E   1C                   ENDIF
062300970526     C*
062400970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
062500970526IF  1C     *IN99         IFEQ      *ON
062600970526     C                   Z-ADD     1             SFLEOF
062700970526E   1C                   ENDIF
062800970526     C*
062900970526     C                   ENDSR
063000970526     C*------------------------------------------------------------------------*
063100970526     C* LETFIL - LETTURA RECORD
063200970526     C*------------------------------------------------------------------------*
063300970526     C     LETFIL        BEGSR
063400970526     C*
063500970526     C* LEGGE FINO A:
063600970526     C*     - FINE FILE
063700970526     C*     - TROVATO RECORD VALIDO
063800970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
063900970526DO  1C     *IN99         DOWEQ     *OFF
064000970526     C     WRECOK        ANDEQ     'N'
064100970526     C*
064200970526     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO DI RICERCA
064300160830     C                   SELECT
064400160831     C*
064500161108IF  1C     WTIPRI        WHENEQ    *blank
064600160831     C* i dati li reperisce mediante SQL
064700160901     C                   EXSR      Legge_SQL
064800160830     C*
064900160830E   2C                   ENDSL
065000970526     C*
065100970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
065200970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
065300970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
065400970530X   2C                   ELSE
065500970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
065600970530E   2C                   ENDIF
065700970526E   1C                   ENDDO
065800970526     C*
065900970526     C                   ENDSR
066000970526     C*------------------------------------------------------------------------*
066100970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
066200970526     C*------------------------------------------------------------------------*
066300970526     C     CHKREC        BEGSR
066400970526     C*
066500970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
066600160913     C*
066700160919     C* Proc diversa
066800161108IF  1C*                  if        wDrillUp  =  *off and
066900161108IF  1C*                            D03Proc<> *blank and
067000161108IF  1C*                            D03Proc<> VMSProc
067100161108     C*                  MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
067200161108     C*                  leavesr
067300161108     C*                  endif
067400161108
067500160713
067600970526     C                   ENDSR
067700970526     C*------------------------------------------------------------------------*
067800160830     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE            §
067900970526     C*------------------------------------------------------------------------*
068000970526     C     IRESFL        BEGSR
068100970526     C*
068200970526     C                   MOVEL     *BLANKS       OPZKEY
068300161111
068400161130         select;
068500161130
068600170201          // tengo la gestione drill anche se PER ORA non è usata
068700170201          when wLivDrill = 1;
068800170201           D05Logica = WAFOB_fetch1.Logica;
068900170202           D05DtEstr = %char(WAFOB_fetch1.DtEstr:*eur);
069000170307           D05DtEstrI = WAFOB_fetch1.DtEstr;
069100170201           D05Lib    = WAFOB_fetch1.Lib;
069200170201           D05ObjT   = WAFOB_fetch1.ObjT;
069300170201           D05Obj    = WAFOB_fetch1.Obj;
069400170201           D05Attr   = WAFOB_fetch1.Attr;
069500170306           D05UtC    = %subst(WAFOB_fetch1.Dati:1:10);
069600170306           D05Dati   = WAFOB_fetch1.Dati;
069700161130
069800161130         endsl;
069900160901     C*
070000970526     C                   ENDSR
070100161115     C*------------------------------------------------------------------------*
070200161115     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
070300161115     C*------------------------------------------------------------------------*
070400161115     C     CHKPAR        BEGSR
070500161115     C*
070600970526     C* REIMPOSTO FLAG EVIDENZAZIONE ERRORI SUI PARAMETRI
070700970526     C                   SETOFF                                       808182
070800160913     C                   SETOFF                                       838485
070900970526     C*--------------------
071000970526     C* ORDINAMENTO
071100970526     C*--------------------
071200970526     C*--------------------
071300970526     C* SELEZIONI
071400970526     C*--------------------
071500970526     C*
071600160831     C* SE NESSUN ORDINAMENTO RICHIESTO, utilizzo il caricamento completo per Procedura
071700160830     C*
071800161108     C                   EVAL      WTIPRI = *blank
071900160830IF  1C     ERRL00        IFEQ      '0'
072000161108     C*                  SELECT
072100161108E   2C*                  ENDSL
072200970526E   1C                   ENDIF
072300970526     C*
072400970526     C                   ENDSR
072500970526     C*------------------------------------------------------------------------*
072600970526     C* LANPGM - LANCIA UN PROGRAMMA
072700970526     C*------------------------------------------------------------------------*
072800970526     C     LANPGM        BEGSR
072900970526     C*
073000970526     C* IMPOSTA LA DS DI PROCEDURA
073100170301     C                   select
073200170301     C                   when      D05Logica = '01'
073300170301     C                   MOVEL     WOP0          BA01OP0                         *LIVELLO PROCEDURA
073400170301     C                   MOVEL     WOP1          BA01OP1                         *LIVELLO PGM
073500170301     C                   MOVEL     '0'           BA01F03                         *PREMUTO F03=FINE
073600170301     C                   MOVEL     '0'           BA01F12                         *PREMUTO F12=RIT
073700170301     C                   MOVEL     '0'           BA01ERR                         *'1'=ERRORE
073800170301     C                   MOVEL     *BLANKS       BA01MSG                         *MESSAGGIO DI RIT
073900970526     C*
074000970526     C                   CALL      WPGM
074100970612     C                   PARM                    KPJBA
074200170301     C                   PARM                    UBLGOBA01S
074300970526     C*
074400970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
074500170202IF  1C     GOBAF03       IFEQ      '1'                                          *PREMUTO F03
074600970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
074700970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
074800970526X   1C                   ELSE                                                   *NON PREMUTO F03
074900170301     C                   MOVEL     BA01MSG       DSMSMS                         *MESSAGGIO DI RIT
075000970526E   1C                   ENDIF
075100970526     C*
075200170323     C                   when      D05Logica = '02'
075300170323     C                   MOVEL     WOP0          BA02OP0                         *LIVELLO PROCEDURA
075400170323     C                   MOVEL     WOP1          BA02OP1                         *LIVELLO PGM
075500170323     C                   MOVEL     '0'           BA02F03                         *PREMUTO F03=FINE
075600170323     C                   MOVEL     '0'           BA02F12                         *PREMUTO F12=RIT
075700170323     C                   MOVEL     '0'           BA02ERR                         *'1'=ERRORE
075800170323     C                   MOVEL     *BLANKS       BA02MSG                         *MESSAGGIO DI RIT
075900170323     C*
076000170323     C                   CALL      WPGM
076100170323     C                   PARM                    KPJBA
076200170323     C                   PARM                    UBLGOBA02S
076300170323     C*
076400170323     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
076500170323IF  1C     GOBAF03       IFEQ      '1'                                          *PREMUTO F03
076600170323     C                   MOVEL     '1'           ERRL00                         *ERRORE
076700170323     C                   MOVEL     '1'           F03L00                         *FINE PGM
076800170323X   1C                   ELSE                                                   *NON PREMUTO F03
076900170323     C                   MOVEL     BA02MSG       DSMSMS                         *MESSAGGIO DI RIT
077000170323E   1C                   ENDIF
077100170323     C*
077200170301     C                   ENDSL
077300170301     C*
077400970526     C                   ENDSR
077500970527     C*------------------------------------------------------------------------*
077600970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
077700970527     C*------------------------------------------------------------------------*
077800970527     C     CARTAB        BEGSR
077900970527     C*
078000970527     C                   ENDSR
078100970526     C*------------------------------------------------------------------------*
078200970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
078300970526     C*------------------------------------------------------------------------*
078400970526     C     CHKFOP        BEGSR
078500970626     C*
078600970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
078700970626IF  1C     DEPOP0        IFEQ      'R01'
078800161111     C***                MOVEL     OPD(1)        OPDRIG                         *SCELTA
078900161111     C***                MOVEL     'S'           OPDATT
079000161111     C***                MOVEL     OPDRIG        OPD(1)
079100970626X   1C                   ELSE
079200970626     C* OPZIONI
079300170301     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
079400170301     C                   MOVEL     'S'           OPDATT
079500170301     C                   MOVEL     OPDRIG        OPD(5)
079600170302     C                   MOVEL     OPD(14)       OPDRIG                         *INTERROGAZIONE
079700170302     C                   MOVEL     'S'           OPDATT
079800170302     C                   MOVEL     OPDRIG        OPD(14)
079900970626     C* FUNZIONI
080000970626E   1C                   ENDIF
080100970626     C*
080200000602     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
080300970626IF  1C     DEPOP0        IFEQ      'R02'
080400161111     C***                MOVEL     OPD(1)        OPDRIG                         *MODIFICA
080500161111     C***                MOVEL     'S'           OPDATT
080600161111     C***                MOVEL     OPDRIG        OPD(1)
080700161111     C***                MOVEL     OPD(2)        OPDRIG                         *MODIFICA
080800161111     C***                MOVEL     'S'           OPDATT
080900161111     C***                MOVEL     OPDRIG        OPD(2)
081000170327     C                   MOVEL     OPD(3)        OPDRIG                         *MODIFICA
081100170327     C                   MOVEL     'S'           OPDATT
081200170327     C                   MOVEL     OPDRIG        OPD(3)
081300170327     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
081400170327     C                   MOVEL     'S'           OPDATT
081500170327     C                   MOVEL     OPDRIG        OPD(4)
081600170327     C                   MOVEL     OPD(11)       OPDRIG                         *MODIFICA
081700170327     C                   MOVEL     'S'           OPDATT
081800170327     C                   MOVEL     OPDRIG        OPD(11)
081900970715     C*
082000161111     C* SOLO SE EDP ABILITA  INSERIMENTO
082100970715IF  2C     UTENTE        IFEQ      'EDP'
082200170327     C                   MOVEL     OPD(3)        OPDRIG                         *COPIA
082300170327     C                   MOVEL     'S'           OPDATT
082400170327     C                   MOVEL     OPDRIG        OPD(3)
082500161111     C***                MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
082600161111     C***                MOVEL     'S'           FUDATT
082700161111     C***                MOVEL     FUDRIG        FUD(6)
082800170327     C                   MOVEL     OPD(11)       OPDRIG                         *COPIA
082900170327     C                   MOVEL     'S'           OPDATT
083000170327     C                   MOVEL     OPDRIG        OPD(11)
083100970715E   2C                   ENDIF
083200970715E   1C                   ENDIF
083300970626     C*
083400000602     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO, ALLINEAMENTO
083500020919IF  1C     DUTLPO        IFNE      'S'                                          *FILIALE
083600970626     C                   MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
083700970626     C                   MOVEL     'N'           OPDATT
083800970626     C                   MOVEL     OPDRIG        OPD(4)
083900970626E   1C                   ENDIF
084000970526     C*
084100970526     C* GESTIONE OPZIONI A VIDEO
084200970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
084300970526     C                   EXSR      OPZGES
084400970526     C*
084500970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
084600970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
084700970526     C                   EXSR      FUNGES
084800970526     C*
084900970526     C                   ENDSR
085000020919     C*--------------------------------------------------------------------------------------------*
085100020919     C* REPERISCE I DATI UTENTE
085200020919     C*--------------------------------------------------------------------------------------------*
085300020919     C     REPDATIUTE    BEGSR
085400020919     C*
085500020919     C* INIZIALIZZA VARIABILI DI WRK
085600020919     C                   CLEAR                   TIBS34DS
085700020919     C                   CLEAR                   AZUTEDS
085800020919     C                   CLEAR                   DDATIUTE
085900020919     C*
086000020919     C     *DTAARA       DEFINE    §azute        azuteds
086100020919     C     *DTAARA       DEFINE    §datiute      ddatiute
086200020919     C                   IN(E)     *DTAARA
086300020919if  1C                   IF        %Error
086400020919     c                   EVAL      I34Tla = 'L'
086500020919     C                   CALL      'TIBS34R'
086600020919     C                   PARM                    Tibs34Ds
086700020919     C                   IN        *DTAARA
086800020919e   1C                   ENDIF
086900020919     C*
087000020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
087100020919     C                   MOVEL(P)  RSUT          DSFIRS           20
087200020919     C*
087300020919     C                   ENDSR
087400970526     C*------------------------------------------------------------------------*
087500970526     C* *INZSR - OPERAZIONI INIZIALI
087600970526     C*------------------------------------------------------------------------*
087700970526     C     *INZSR        BEGSR
087800970526     C*---
087900970526     C* RICEVIMENTO PARAMETRI
088000970526     C*---
088100970526     C     *ENTRY        PLIST
088200970612     C                   PARM                    KPJBA
088300160714     C*
088400170201     C                   EVAL      UBLGOBADS = kpjbu
088500160913     c                   if        kpjbu = *blank
088600170201     c                   clear                   UBLGOBADS
088700160913     c                   endif
088800970526     C*
088900970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
089000970526     C     XMSGPA        PLIST
089100970526     C                   PARM                    DSMSMS
089200970526     C                   PARM                    DSMSPG
089300970526     C                   PARM                    DSMSKE
089400970526     C                   PARM                    DSMSER
089500970526     C*
089600970526     C* IMPOSTA LA DS -TASTI PREMUTI-
089700970526     C                   CALL      'XKEY'
089800970526     C                   PARM                    DSKEY
089900970526     C*---
090000970526     C* VARIABILI NON RIFERITE AL DATA BASE
090100970526     C*---
090200970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
090300970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
090400970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
090500970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
090600970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
090700970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
090800970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
090900970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
091000970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
091100170202     C     *LIKE         DEFINE    GOBAOP0       DEPOP0                         *DI DEPOSITO
091200170202     C     *LIKE         DEFINE    GOBAOP1       DEPOP1
091300170202     C     *LIKE         DEFINE    GOBAOP0       WOP0                           *DI LAVORO
091400170202     C     *LIKE         DEFINE    GOBAOP1       WOP1
091500970526     C*---
091600970526     C* VARIABILI DI CONTROLLO
091700970526     C*---
091800970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
091900970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
092000970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
092100970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
092200970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
092300170301     C                   Z-ADD     15            SFLPAG                         *N°RIGHE X PAG
092400970526     C*
092500970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
092600170201     C                   MOVEL     'UBLGOBAR'    DSMSPG
092700170201     C                   MOVEL     'UBLGOBAR'    MSGQUE
092800970526     C                   MOVEL     *BLANKS       MSGKEY
092900970526     C                   MOVEL     *BLANKS       DSMSMS
093000970526     C                   MOVEL     *BLANKS       DSMSKE
093100970526     C                   Z-ADD     *ZEROS        DSMSER
093200970526     C*---
093300970526     C* VARIABILI RIFERITE AL DATA BASE
093400970526     C*---
093500970526     C*---
093600970526     C* CHIAVI DI LETTURA
093700970526     C*---
093800970527     C*
093900970527     C* REPERIMENTO DATI UTENTE
094000020919     C                   EXSR      REPDATIUTE
094100970527     C*
094200970527     C* CARICA LE TABELLE OCCORRENTI
094300970527     C                   EXSR      CARTAB
094400970526     C*
094500970526     C* DEPOSITA LE OPZIONI DI ENTRATA
094600170202     C                   MOVEL     GOBAOP0       DEPOP0                         *LIVELLO PROCEDURA
094700170202     C                   MOVEL     GOBAOP1       DEPOP1                         *LIVELLO PROGRAMMA
094800970526     C*
094900970526     C* CALCOLA LA DATA CORRENTE
095000970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
095100970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
095200970526     C                   Z-ADD     WN8           G08DAT
095300970526     C                   Z-ADD     *ZEROS        G08INV
095400970526     C                   MOVEL     '0'           G08ERR
095500970526     C                   CALL      'XSRDA8'
095600970526     C                   PARM                    WLBDA8
095700970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
095800970526     C*
095900970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
096000970526     C                   EXSR      CHKFOP
096100970526     C*
096200970526     C* IMPOSTA I PARAMETRI DI INGRESSO
096300970526     C*
096400161108     C*
096500970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
096600970530IF  1C     DEPOP0        IFEQ      'R01'
096700970530     C                   MOVEL     DEC(3)        VIDING
096800970530E   1C                   ENDIF
096900970530IF  1C     DEPOP0        IFEQ      'R02'
097000970530     C                   MOVEL     DEC(1)        VIDING
097100970530E   1C                   ENDIF
097200970530IF  1C     DEPOP0        IFEQ      'R05'
097300970530     C                   MOVEL     DEC(2)        VIDING
097400970530E   1C                   ENDIF
097500970526     C*
097600970526     C                   ENDSR
097700161110
097800970526     C*------------------------------------------------------------------------*
097900970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
098000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
098100970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
098200970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
098300970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
098400970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
098500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
098600970526     C*------------------------------------------------------------------------*
098700970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
098800970526     C*------------------------------------------------------------------------*
098900970526     C     POSSFL        BEGSR
099000970526     C*
099100970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
099200970526     C                   EXSR      CLRSFL
099300970526     C*
099400170118     C**************************************************************************
099500170118     C* siccome dentro la routine CARSFL c'è il richiamo alla routine SETFIL se
099600170118     C* non sono stati caricati rcd sul sfl (che vengono caricati dalla routine
099700170118     C* CARSFL dopo il richiamo alla SETFIL),
099800170118     C* non ha senso chiamare prima la SETFIL, faccio fare tutto alla CARSFL
099900170118     C**************************************************************************
100000970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
100100170118     C***                EXSR      SETFIL
100200970526     C*
100300970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
100400170118     C***  SFLEOF        IFEQ      0
100500170118     C***                EXSR      CARSFL
100600170118     C***                END
100700170118     C**************************************************************************
100800170118     C*
100900170118     C                   EXSR      CARSFL
101000970526     C*
101100970526     C                   ENDSR
101200970526     C*------------------------------------------------------------------------*
101300970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
101400970526     C*------------------------------------------------------------------------*
101500970526     C     ENTATT        BEGSR
101600970526     C*
101700970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
101800970526     C                   Z-ADD     0             OPZOK             1 0
101900970526     C*
102000970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
102100970526     C                   Z-ADD     0             SFLVAR            5 0
102200970526     C*
102300970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
102400970526     C                   Z-ADD     0             SFLERR
102500970526     C*
102600970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
102700970526     C                   Z-ADD     SFLULT        WFLULT
102800970526     C*
102900970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
103000170201     C                   READC     LGOBD05                                98
103100970526     C     *IN98         DOWEQ     '0'
103200970526     C*
103300970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
103400970526     C                   ADD       1             SFLVAR
103500970526     C*
103600970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
103700970526     C     OPZOK         IFEQ      0
103800970526     C*
103900970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
104000970526     C     OPZKEY        IFNE      *BLANKS
104100970526     C                   EXSR      OPZCON
104200970526     C                   END
104300970526     C*
104400970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
104500970526     C*            2. PULIZIA SCELTA IN SFL
104600970526     C     OPZOK         IFEQ      0
104700970526     C                   MOVEL     '0'           *IN41
104800970526     C                   MOVEL     *BLANKS       OPZKEY
104900970526     C                   ELSE
105000970526     C*
105100970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
105200970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
105300970526     C                   MOVEL     '1'           *IN41
105400970526     C                   Z-ADD     SFLULT        SFLERR
105500970526     C                   END
105600970526     C                   ELSE
105700970526     C*
105800970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
105900970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
106000970526     C                   MOVEL     '1'           *IN41
106100970526     C                   END                                                    OPZOK=0
106200970526     C*
106300970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
106400970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
106500970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
106600970526     C     OPZOK         IFNE      0
106700970526     C                   Z-ADD     SFLERR        SFLNBR
106800970526     C                   ELSE
106900970526     C                   Z-ADD     SFLULT        SFLNBR
107000970526     C                   END
107100970526     C*
107200970526     C* AGGIORNO RECORD SFL
107300160902     C*  imposto "effetti speciali"
107400160902     C                   EXSR      SFXD05
107500170201     C                   UPDATE    LGOBD05
107600970526     C*
107700170201     C                   READC     LGOBD05                                98
107800970526     C                   END                                                    DO WHILE
107900970526     C*
108000970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
108100970526     C                   Z-ADD     WFLULT        SFLULT
108200970526     C*
108300970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
108400970526     C     SFLVAR        IFEQ      0
108500970526     C*---                 MOVEL'1'       FINL00
108600970526     C                   END
108700970526     C*
108800970526     C                   ENDSR
108900970526     C*------------------------------------------------------------------------*
109000970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
109100970526     C*------------------------------------------------------------------------*
109200970526     C     CLRSFL        BEGSR
109300970526     C*
109400970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
109500970526     C* (0=NON FINE FILE  1=FINE FILE)
109600970526     C                   Z-ADD     0             SFLEOF
109700970526     C*
109800970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
109900970526     C                   Z-ADD     0             SFLULT
110000970526     C*
110100970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
110200970526     C                   MOVEL     '1'           *IN40
110300170201     C                   WRITE     LGOBD06
110400970526     C                   MOVEL     '0'           *IN40
110500970526     C*
110600970526     C                   ENDSR
110700970526     C*------------------------------------------------------------------------*
110800970526     C* CARSFL - CARICAMENTO SFL
110900970526     C*------------------------------------------------------------------------*
111000970526     C     CARSFL        BEGSR
111100970526     C*
111200970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
111300970526     C     SFLULT        IFEQ      0
111400970526     C                   EXSR      SETFIL
111500970526     C                   END
111600970526     C*
111700970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
111800970526     C                   Z-ADD     SFLULT        WFLULT
111900970526     C*
112000970526     C* CICLO:
112100970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
112200970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
112300970526     C                   Z-ADD     1             SFLRIG
112400970526     C     SFLRIG        DOWLE     SFLPAG
112500970526     C     SFLEOF        ANDEQ     0
112600970526     C*
112700970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
112800970526     C                   MOVEL     *BLANKS       OPZKEY
112900970526     C*
113000970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
113100970526     C                   EXSR      IRESFL
113200970526     C*
113300970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
113400970526     C                   ADD       1             SFLULT
113500970526     C*
113600970526     C* SCRITTURA RECORD SFL
113700160902     C*  imposto "effetti speciali"
113800160902     C                   EXSR      SFXD05
113900170201     C                   WRITE     LGOBD05
114000970526     C                   ADD       1             SFLRIG
114100970526     C*
114200970526     C* LETTURA FILE DA CARICARE
114300970526     C                   EXSR      LETFIL
114400970526     C                   END                                                    DO WHILE
114500970526     C*
114600970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
114700970526     C     WFLULT        IFNE      SFLULT
114800970526     C                   Z-ADD     WFLULT        SFLNBR
114900970526     C                   ADD       1             SFLNBR
115000970526     C                   END
115100970526     C*
115200970526     C                   ENDSR
115300970526     C*------------------------------------------------------------------------*
115400160902     C* SFXD05 - effetti speciali sfl D05 (RI, ND, BL, ...)
115500160902     C*          a seconda dei dati presenti in riga
115600970526     C*------------------------------------------------------------------------*
115700160902     C     SFXD05        BEGSR
115800161110     C*
115900160902     C*
116000160902     C                   ENDSR
116100160902     C*------------------------------------------------------------------------*
116200160902     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
116300160902     C*------------------------------------------------------------------------*
116400160902     C     OUTSFL        BEGSR
116500160902     C*
116600970526     C* WRITE RECORD TESTATA VIDEO
116700170201     C                   WRITE     LGOBD01
116800970526     C*
116900970526     C* VISUALIZZAZIONE MESSAGGIO
117000970526     C                   EXSR      SNDMSG
117100970526     C*
117200970526     C* WRITE RECORD OPZIONI VIDEO
117300170201     C                   WRITE     LGOBD02
117400970526     C*
117500970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
117600170201     C                   WRITE     LGOBD03
117700970526     C*
117800970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
117900170201     C                   WRITE     LGOBD04
118000970526     C*
118100970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
118200970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
118300970526     C     SFLEOF        IFEQ      0
118400970526     C                   MOVEL     '0'           *IN42
118500970526     C                   ELSE
118600970526     C*
118700970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
118800970526     C                   MOVEL     '1'           *IN42
118900970526     C                   END
119000970526     C*
119100970526     C* WRITE RECORD TASTI FUNZIONALI
119200170201     C                   WRITE     LGOBD09
119300970526     C*
119400970526     C* CASO DI SFL VUOTO
119500970526     C     SFLULT        IFLE      0
119600970526     C*
119700970526     C*    - WRITE RECORD VUOTO
119800170201     C                   WRITE     LGOBD0A
119900170201     C                   READ      LGOBD0A                                99
120000970526     C*
120100970526     C                   ELSE
120200970526     C*
120300970526     C* CASO DI SFL CON RECORDS
120400970526     C*
120500970526     C*    - WRITE RECORD CONTROLLO SFL
120600170201     C                   WRITE     LGOBD06
120700170201     C                   READ      LGOBD06                                99
120800170302     C* memo che c'è stata una exfmt del sfl
120900170302     C                   EVAL      wExfmt = *on
121000970526     C                   END
121100970526     C*
121200970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
121300170201     C                   READ      LGOBD03                                99
121400970526     C*
121500970526     C                   ENDSR
121600970526     C*------------------------------------------------------------------------*
121700970526     C* SNDMSG - MANDA MESSAGGIO
121800970526     C*------------------------------------------------------------------------*
121900970526     C     SNDMSG        BEGSR
122000970526     C*
122100970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
122200970526     C                   MOVEL     '1'           *IN40
122300170201     C                   WRITE     LGOBDMC
122400970526     C                   MOVEL     '0'           *IN40
122500970526     C*
122600970526     C* VISUALIZZAZIONE MESSAGGIO
122700970526     C                   Z-ADD     0             MSGULT
122800970526     C                   CALL      'XMSG'        XMSGPA
122900970526     C     DSMSER        IFEQ      0
123000970526     C                   MOVEL     DSMSKE        MSGKEY
123100970526     C                   END
123200970526     C                   Z-ADD     1             MSGULT
123300170201     C                   WRITE     LGOBDMS                                      SFL MESS
123400170201     C                   WRITE     LGOBDMC                                      CONTR MESS
123500970526     C*
123600970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
123700970526     C                   MOVEL     *BLANKS       DSMSMS
123800970526     C*
123900970526     C                   ENDSR
124000970526     C*------------------------------------------------------------------------*
124100970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
124200970526     C*------------------------------------------------------------------------*
124300970526     C     FUNCON        BEGSR
124400970526     C*
124500970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
124600970526     C                   EXSR      FUNVAR
124700970526     C*
124800970526     C* PULISCO CAMPI DI LAVORO
124900970526     C                   Z-ADD     0             I                 5 0
125000970526     C                   Z-ADD     0             W                 5 0
125100970526     C*
125200970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
125300970526     C     FUNKEY        IFNE      *BLANKS
125400970526     C     1             DO        24            I
125500970526     C                   MOVEL     FUD(I)        FUDRIG
125600970526     C*
125700970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
125800970526     C     FUNKEY        IFEQ      FUDFUN
125900970526     C     FUDATT        ANDEQ     'S'
126000970526     C                   Z-ADD     I             W
126100970526     C                   END
126200970526     C                   END                                                    DO WHILE
126300970526     C*
126400970526     C* FUNZIONE NON ATTIVA (ERRORE)
126500970526     C     W             IFEQ      0
126600970526     C*
126700970526     C* EMISSIONE MESSAGGIO DI ERRORE
126800970526     C                   MOVEL     MSG(2)        DSMSMS
126900970526     C                   END                                                    W=0
127000970526     C                   END                                                    FUNKEY NE *BLANKS
127100970526     C*
127200970526     C* FUNZIONE ATTIVA (PROCEDI ...)
127300970526     C     W             IFNE      0
127400970526     C     FUNKEY        OREQ      *BLANKS
127500970526     C*
127600970526     C     FUNKEY        IFEQ      *BLANKS
127700970526     C                   EXSR      ENTGES
127800970526     C                   END
127900970526     C     FUNKEY        IFEQ      '03'
128000970526     C                   EXSR      F03GES
128100970526     C                   END
128200970526     C     FUNKEY        IFEQ      '05'
128300970526     C                   EXSR      F05GES
128400970526     C                   END
128500970526     C     FUNKEY        IFEQ      '06'
128600970526     C                   EXSR      F06GES
128700970526     C                   END
128800161130     C     FUNKEY        IFEQ      '07'
128900161130     C                   EXSR      F07GES
129000161111     C                   END
129100161130     C     FUNKEY        IFEQ      '08'
129200161130     C                   EXSR      F08GES
129300161130     C                   END
129400160913     C     FUNKEY        IFEQ      '12'
129500160913     C                   EXSR      F12GES
129600160913     C                   END
129700970526     C     FUNKEY        IFEQ      '23'
129800970526     C                   EXSR      F23GES
129900970526     C                   END
130000970526     C     FUNKEY        IFEQ      '24'
130100970526     C                   EXSR      F24GES
130200970526     C                   END
130300970526     C     FUNKEY        IFEQ      'RU'
130400970526     C                   EXSR      RUPGES
130500970526     C                   END
130600970526     C*
130700970526     C                   END
130800970526     C*
130900970526     C                   ENDSR
131000970526     C*------------------------------------------------------------------------*
131100970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
131200970526     C*------------------------------------------------------------------------*
131300970526     C     FUNVAR        BEGSR
131400970526     C*
131500970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
131600970526     C     KEY           IFEQ      ENTER
131700970526     C                   MOVEL     *BLANKS       FUNKEY
131800970526     C                   END
131900970526     C     KEY           IFEQ      F1
132000970526     C                   MOVEL     '01'          FUNKEY
132100970526     C                   END
132200970526     C     KEY           IFEQ      F2
132300970526     C                   MOVEL     '02'          FUNKEY
132400970526     C                   END
132500970526     C     KEY           IFEQ      F3
132600970526     C                   MOVEL     '03'          FUNKEY
132700970526     C                   END
132800970526     C     KEY           IFEQ      F4
132900970526     C                   MOVEL     '04'          FUNKEY
133000970526     C                   END
133100970526     C     KEY           IFEQ      F5
133200970526     C                   MOVEL     '05'          FUNKEY
133300970526     C                   END
133400970526     C     KEY           IFEQ      F6
133500970526     C                   MOVEL     '06'          FUNKEY
133600970526     C                   END
133700970526     C     KEY           IFEQ      F7
133800970526     C                   MOVEL     '07'          FUNKEY
133900970526     C                   END
134000970526     C     KEY           IFEQ      F8
134100970526     C                   MOVEL     '08'          FUNKEY
134200970526     C                   END
134300970526     C     KEY           IFEQ      F9
134400970526     C                   MOVEL     '09'          FUNKEY
134500970526     C                   END
134600970526     C     KEY           IFEQ      F10
134700970526     C                   MOVEL     '10'          FUNKEY
134800970526     C                   END
134900970526     C     KEY           IFEQ      F11
135000970526     C                   MOVEL     '11'          FUNKEY
135100970526     C                   END
135200970526     C     KEY           IFEQ      F12
135300970526     C                   MOVEL     '12'          FUNKEY
135400970526     C                   END
135500970526     C     KEY           IFEQ      F13
135600970526     C                   MOVEL     '13'          FUNKEY
135700970526     C                   END
135800970526     C     KEY           IFEQ      F14
135900970526     C                   MOVEL     '14'          FUNKEY
136000970526     C                   END
136100970526     C     KEY           IFEQ      F15
136200970526     C                   MOVEL     '15'          FUNKEY
136300970526     C                   END
136400970526     C     KEY           IFEQ      F16
136500970526     C                   MOVEL     '16'          FUNKEY
136600970526     C                   END
136700970526     C     KEY           IFEQ      F17
136800970526     C                   MOVEL     '17'          FUNKEY
136900970526     C                   END
137000970526     C     KEY           IFEQ      F18
137100970526     C                   MOVEL     '18'          FUNKEY
137200970526     C                   END
137300970526     C     KEY           IFEQ      F19
137400970526     C                   MOVEL     '19'          FUNKEY
137500970526     C                   END
137600970526     C     KEY           IFEQ      F20
137700970526     C                   MOVEL     '20'          FUNKEY
137800970526     C                   END
137900970526     C     KEY           IFEQ      F21
138000970526     C                   MOVEL     '21'          FUNKEY
138100970526     C                   END
138200970526     C     KEY           IFEQ      F22
138300970526     C                   MOVEL     '22'          FUNKEY
138400970526     C                   END
138500970526     C     KEY           IFEQ      F23
138600970526     C                   MOVEL     '23'          FUNKEY
138700970526     C                   END
138800970526     C     KEY           IFEQ      F24
138900970526     C                   MOVEL     '24'          FUNKEY
139000970526     C                   END
139100970526     C     KEY           IFEQ      ROLLUP
139200970526     C                   MOVEL     'RU'          FUNKEY
139300970526     C                   END
139400970526     C     KEY           IFEQ      HELP
139500970526     C                   MOVEL     'HP'          FUNKEY
139600970526     C                   END
139700970526     C*
139800970526     C                   ENDSR
139900970526     C*------------------------------------------------------------------------*
140000970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
140100970526     C*------------------------------------------------------------------------*
140200970526     C     FUNGES        BEGSR
140300970526     C*
140400970526     C* PULISCO CAMPI DI LAVORO
140500970526     C                   Z-ADD     0             W                 5 0
140600970526     C*
140700970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
140800970526     C* E CARICO TABELLA FUNZIONI ATTIVE
140900970526     C     1             DO        24            I
141000970526     C                   MOVEL     FUD(I)        FUDRIG
141100970526     C*
141200970526     C* PULIZIA FUNZIONI PRECEDENTI
141300970526     C                   MOVEL     *BLANKS       FUV(I)
141400970526     C*
141500970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
141600970526     C* (SALTA F23 E F24)
141700970526     C     FUDATT        IFEQ      'S'
141800970526     C     FUDFUN        ANDNE     '23'
141900970526     C     FUDFUN        ANDNE     '24'
142000970526     C                   ADD       1             W
142100970526     C                   MOVEL     FUDDES        FUV(W)
142200970526     C                   END
142300970526     C                   END                                                    DO WHILE
142400970526     C*
142500970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
142600970526     C* ALTRIMENTI DISATTIVO F24
142700970526     C     1             DO        24            I
142800970526     C                   MOVEL     FUD(I)        FUDRIG
142900970526     C*
143000970526     C     FUDFUN        IFEQ      '24'
143100970526     C     W             IFGT      6
143200970526     C                   MOVEL     'S'           FUDATT
143300970526     C                   ELSE
143400970526     C                   MOVEL     *BLANKS       FUDATT
143500970526     C                   END                                                    W GT 8
143600970526     C                   MOVEL     FUDRIG        FUD(I)
143700970526     C                   END
143800970526     C                   END                                                    DO WHILE
143900970526     C*
144000970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
144100970526     C     W             DIV       6             I
144200970526     C                   MVR                     Z
144300970526     C*
144400970526     C     Z             IFNE      0
144500970526     C     I             ADD       1             Z
144600970526     C                   ELSE
144700970526     C                   Z-ADD     I             Z
144800970526     C                   END
144900970526     C*
145000970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
145100970526     C                   ADD       1             FUNGRU
145200970526     C     FUNGRU        IFGT      Z
145300970526     C     FUNGRU        ORLT      1
145400970526     C                   Z-ADD     1             FUNGRU
145500970526     C                   END
145600970526     C*
145700970526     C* PRIMO GRUPPO DI 6 FUNZIONI
145800970526     C     FUNGRU        IFEQ      1
145900970526     C                   MOVEL     FUV(1)        FUN001
146000970526     C                   MOVEL     FUV(2)        FUN002
146100970526     C                   MOVEL     FUV(3)        FUN003
146200970526     C                   MOVEL     FUV(4)        FUN004
146300970526     C                   MOVEL     FUV(5)        FUN005
146400970526     C                   MOVEL     FUV(6)        FUN006
146500970526     C                   END
146600970526     C*
146700970526     C* SECONDO GRUPPO DI 6 FUNZIONI
146800970526     C     FUNGRU        IFEQ      2
146900970526     C                   MOVEL     FUV(7)        FUN001
147000970526     C                   MOVEL     FUV(8)        FUN002
147100970526     C                   MOVEL     FUV(9)        FUN003
147200970526     C                   MOVEL     FUV(10)       FUN004
147300970526     C                   MOVEL     FUV(11)       FUN005
147400970526     C                   MOVEL     FUV(12)       FUN006
147500970526     C                   END
147600970526     C*
147700970526     C* TERZO GRUPPO DI 6 FUNZIONI
147800970526     C     FUNGRU        IFEQ      3
147900970526     C                   MOVEL     FUV(13)       FUN001
148000970526     C                   MOVEL     FUV(14)       FUN002
148100970526     C                   MOVEL     FUV(15)       FUN003
148200970526     C                   MOVEL     FUV(16)       FUN004
148300970526     C                   MOVEL     FUV(17)       FUN005
148400970526     C                   MOVEL     FUV(18)       FUN006
148500970526     C                   END
148600970526     C*
148700970526     C* QUARTO GRUPPO DI 6 FUNZIONI
148800970526     C     FUNGRU        IFEQ      4
148900970526     C                   MOVEL     FUV(19)       FUN001
149000970526     C                   MOVEL     FUV(20)       FUN002
149100970526     C                   MOVEL     FUV(21)       FUN003
149200970526     C                   MOVEL     FUV(22)       FUN004
149300970526     C                   MOVEL     *BLANKS       FUN005
149400970526     C                   MOVEL     *BLANKS       FUN006
149500970526     C                   END
149600970526     C*
149700970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
149800970526     C                   MOVEL     *BLANKS       FUN007
149900970526     C                   MOVEL     *BLANKS       FUN008
150000970526     C*
150100970526     C     1             DO        24            I
150200970526     C                   MOVEL     FUD(I)        FUDRIG
150300970526     C     FUDFUN        IFEQ      '23'
150400970526     C     FUDATT        ANDEQ     'S'
150500970526     C                   MOVEL     FUDDES        FUN007
150600970526     C                   END
150700970526     C     FUDFUN        IFEQ      '24'
150800970526     C     FUDATT        ANDEQ     'S'
150900970526     C                   MOVEL     FUDDES        FUN008
151000970526     C                   END
151100970526     C                   END                                                    DO
151200970526     C*
151300970526     C                   ENDSR
151400970526     C*------------------------------------------------------------------------*
151500970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
151600970526     C*------------------------------------------------------------------------*
151700970526     C     OPZGES        BEGSR
151800970526     C*
151900970526     C* PULISCO CAMPI DI LAVORO
152000970526     C                   Z-ADD     0             W                 5 0
152100970526     C*
152200970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
152300970526     C* E CARICO TABELLA OPZIONI ATTIVE
152400970526     C     1             DO        24            I
152500970526     C                   MOVEL     OPD(I)        OPDRIG
152600970526     C*
152700970526     C* PULIZIA OPZIONI PRECEDENTI
152800970526     C                   MOVEL     *BLANKS       OPV(I)
152900970526     C*
153000970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
153100970526     C     OPDATT        IFEQ      'S'
153200970526     C                   ADD       1             W
153300970526     C                   MOVEL     OPDDES        OPV(W)
153400970526     C                   END
153500970526     C                   END                                                    DO WHILE
153600970526     C*
153700970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
153800970526     C* ALTRIMENTI DISATTIVO F23
153900970526     C     1             DO        24            I
154000970526     C                   MOVEL     FUD(I)        FUDRIG
154100970526     C*
154200970526     C     FUDFUN        IFEQ      '23'
154300970526     C     W             IFGT      8
154400970526     C                   MOVEL     'S'           FUDATT
154500970526     C                   ELSE
154600970526     C                   MOVEL     *BLANKS       FUDATT
154700970526     C                   END                                                    W GT 8
154800970526     C                   MOVEL     FUDRIG        FUD(I)
154900970526     C                   END
155000970526     C                   END                                                    DO WHILE
155100970526     C*
155200970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
155300970526     C     W             DIV       8             I
155400970526     C                   MVR                     Z                 5 0
155500970526     C*
155600970526     C     Z             IFNE      0
155700970526     C     I             ADD       1             Z
155800970526     C                   ELSE
155900970526     C                   Z-ADD     I             Z
156000970526     C                   END
156100970526     C*
156200970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
156300970526     C                   ADD       1             OPZGRU
156400970526     C     OPZGRU        IFGT      Z
156500970526     C     OPZGRU        ORLT      1
156600970526     C                   Z-ADD     1             OPZGRU
156700970526     C                   END
156800970526     C*
156900970526     C* PRIMO GRUPPO DI 8 OPZIONI
157000970526     C     OPZGRU        IFEQ      1
157100970526     C                   MOVEL     OPV(1)        OPZ001
157200970526     C                   MOVEL     OPV(2)        OPZ002
157300970526     C                   MOVEL     OPV(3)        OPZ003
157400970526     C                   MOVEL     OPV(4)        OPZ004
157500970526     C                   MOVEL     OPV(5)        OPZ005
157600970526     C                   MOVEL     OPV(6)        OPZ006
157700970526     C                   MOVEL     OPV(7)        OPZ007
157800970526     C                   MOVEL     OPV(8)        OPZ008
157900970526     C                   END
158000970526     C*
158100970526     C* SECONDO GRUPPO DI 8 OPZIONI
158200970526     C     OPZGRU        IFEQ      2
158300970526     C                   MOVEL     OPV(9)        OPZ001
158400970526     C                   MOVEL     OPV(10)       OPZ002
158500970526     C                   MOVEL     OPV(11)       OPZ003
158600970526     C                   MOVEL     OPV(12)       OPZ004
158700970526     C                   MOVEL     OPV(13)       OPZ005
158800970526     C                   MOVEL     OPV(14)       OPZ006
158900970526     C                   MOVEL     OPV(15)       OPZ007
159000970526     C                   MOVEL     OPV(16)       OPZ008
159100970526     C                   END
159200970526     C*
159300970526     C* TERZO GRUPPO DI 8 OPZIONI
159400970526     C     OPZGRU        IFEQ      3
159500970526     C                   MOVEL     OPV(17)       OPZ001
159600970526     C                   MOVEL     OPV(18)       OPZ002
159700970526     C                   MOVEL     OPV(19)       OPZ003
159800970526     C                   MOVEL     OPV(20)       OPZ004
159900970526     C                   MOVEL     OPV(21)       OPZ005
160000970526     C                   MOVEL     OPV(22)       OPZ006
160100970526     C                   MOVEL     OPV(23)       OPZ007
160200970526     C                   MOVEL     OPV(24)       OPZ008
160300970526     C                   END
160400970526     C*
160500970526     C                   ENDSR
160600970526     C*------------------------------------------------------------------------*
160700970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
160800970526     C*------------------------------------------------------------------------*
160900970526     C     OPZCON        BEGSR
161000970526     C*
161100970526     C* PULISCO CAMPI DI LAVORO
161200970526     C                   Z-ADD     0             I                 5 0
161300970526     C                   Z-ADD     0             W                 5 0
161400970526     C*
161500970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
161600970526     C     1             DO        24            I
161700970526     C                   MOVEL     OPD(I)        OPDRIG
161800970526     C*
161900970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
162000970526     C     OPZKEY        IFEQ      OPDOPZ
162100970526     C     OPZKEY        OREQ      OPDOP1
162200970526     C*
162300970526     C     OPDATT        IFEQ      'S'
162400970526     C                   MOVEL     OPDOPZ        OPZKEY
162500970526     C                   Z-ADD     I             W
162600970526     C                   END
162700970526     C                   END
162800970526     C*
162900970526     C                   END                                                    DO WHILE
163000970526     C*
163100970526     C* OPZIONE NON ATTIVA (ERRORE)
163200970526     C     W             IFEQ      0
163300970526     C*
163400970526     C* OPZIONE NON OK
163500970526     C                   Z-ADD     1             OPZOK
163600970526     C*
163700970526     C* EMISSIONE MESSAGGIO DI ERRORE
163800970526     C                   MOVEL     MSG(1)        DSMSMS
163900970526     C                   ELSE
164000970526     C*
164100970526     C* OPZIONE ATTIVA (PROCEDI ...)
164200970526     C*
164300970526     C     OPZKEY        IFEQ      ' 1'
164400970526     C                   EXSR      O01GES
164500970526     C                   END
164600970526     C     OPZKEY        IFEQ      ' 2'
164700970526     C                   EXSR      O02GES
164800970526     C                   END
164900970603     C     OPZKEY        IFEQ      ' 3'
165000970603     C                   EXSR      O03GES
165100970603     C                   END
165200970526     C     OPZKEY        IFEQ      ' 4'
165300970526     C                   EXSR      O04GES
165400970526     C                   END
165500970526     C     OPZKEY        IFEQ      ' 5'
165600970526     C                   EXSR      O05GES
165700970526     C                   END
165800970526     C     OPZKEY        IFEQ      ' 7'
165900970526     C                   END
166000970528     C     OPZKEY        IFEQ      ' 8'
166100970528     C                   END
166200970528     C     OPZKEY        IFEQ      ' 9'
166300970528     C                   END
166400970528     C     OPZKEY        IFEQ      '10'
166500970528     C                   END
166600170327     C     OPZKEY        IFEQ      '11'
166700170327     C                   EXSR      O11GES
166800970528     C                   END
166900170327     C     OPZKEY        IFEQ      '14'
167000170327     C                   EXSR      O14GES
167100170327     C                   END
167200970526     C                   END                                                    OPZ.NON ATTIVA
167300970526     C*
167400970526     C                   ENDSR
167500160831
167600160831       /free
167700160831
167800160831       //--------------------------------------------------------------
167900160831       //?Preparazione stringa SQL                                     ?
168000160831       //--------------------------------------------------------------
168100160831       BEGSR  Prepara_SQL;
168200160831
168300160901         // Chiudo il cursore (lo faccio in anticipo perché non saprei quando chiuderlo
168400160901         exec sql  close C1;
168500160901
168600161130         select;
168700161130
168800170202          // tengo la gestione drill anche se PER ORA non è usata
168900170202          when wLivDrill = 1;
169000170202           wSQL = 'select foblogica, fobdtestr, foblib, fobobjt, fobobj, +
169100170202                   fobattr, fobdati +
169200170202                   from wafob00f';
169300170117           // costruisce la where
169400170117           exsr CreateWhere;
169500161111           wSQL = %trim(wSQL) +
169600170202                  ' order by foblogica, foblib, fobobjt, fobobj';
169700161130
169800161130         endsl;
169900160831
170000160831         // Dichiarazione cursore
170100160831         exec sql   prepare S1   from :wSQL;
170200160831         exec sql   declare C1  asensitive   cursor for S1;
170300160831
170400160831         // Apertura del cursore
170500160831         exec sql   open C1;
170600160831
170700160901         *in99 = *off;
170800160901         if SQLCode < 0;
170900160901           *in99 = *on;
171000160831         endif;
171100160831
171200160831       ENDSR;
171300160831
171400160831       //--------------------------------------------------------------
171500170117       //?Costruisce la where a seconda delle selezioni                ?
171600160831       //--------------------------------------------------------------
171700170117       BEGSR  CreateWhere;
171800160831
171900170117         // se è stato indicato almeno una selezione, allora avrò una where
172000170201         if D03Logica <> *blank
172100170201         or D03Lib    <> *blank
172200170201         or D03ObjT   <> *blank
172300170201         or D03Obj    <> *blank
172400170201         or D03Attr   <> *blank
172500170306         or D03UtC    <> *blank
172600170306         or D03Dati   <> *blank;
172700170117           wSQL = %trim(wSQL) +
172800170117                  ' where';
172900170201           if D03Logica <> *blank;
173000170117           wSQL = %trim(wSQL) +
173100170201                  ' FOBLogica = ''' +
173200170201                  D03Logica +
173300170118                  '''';
173400170117           endif;
173500170201           if D03Lib <> *blank;
173600170202             if D03Logica <> *blank;
173700170118               wSQL = %trim(wSQL) +
173800170118               ' and';
173900170118             endif;
174000170201             wSQL = %trim(wSQL) +
174100170201                  ' FOBLib like ''%' +
174200170202                  %trim(D03Lib) +
174300170201                  '%''';
174400170118           endif;
174500170201           if D03ObjT <> *blank;
174600170202             if D03Logica <> *blank
174700170201             or D03Lib <> *blank;
174800170118               wSQL = %trim(wSQL) +
174900170118               ' and';
175000170118             endif;
175100170201             wSQL = %trim(wSQL) +
175200170201                  ' FOBObjT like ''%' +
175300170202                  %trim(D03ObjT) +
175400170201                  '%''';
175500170118           endif;
175600170201           if D03Obj <> *blank;
175700170202             if D03Logica <> *blank
175800170201             or D03Lib <> *blank
175900170201             or D03ObjT <> *blank;
176000170201               wSQL = %trim(wSQL) +
176100170201               ' and';
176200170201             endif;
176300170201             wSQL = %trim(wSQL) +
176400170201                  ' FOBObj like ''%' +
176500170202                  %trim(D03Obj) +
176600170201                  '%''';
176700170201           endif;
176800170201           if D03Attr <> *blank;
176900170202             if D03Logica <> *blank
177000170201             or D03Lib <> *blank
177100170201             or D03ObjT <> *blank
177200170201             or D03Obj <> *blank;
177300170201               wSQL = %trim(wSQL) +
177400170201               ' and';
177500170201             endif;
177600170201             wSQL = %trim(wSQL) +
177700170201                  ' FOBAttr like ''%' +
177800170202                  %trim(D03Attr) +
177900170201                  '%''';
178000170201           endif;
178100170306           if D03UtC <> *blank;
178200170202             if D03Logica <> *blank
178300170201             or D03Lib <> *blank
178400170201             or D03ObjT <> *blank
178500170201             or D03Obj <> *blank
178600170201             or D03Attr <> *blank;
178700170201               wSQL = %trim(wSQL) +
178800170201               ' and';
178900170201             endif;
179000170201             wSQL = %trim(wSQL) +
179100170306                  ' substr(FOBDati, 1 , 10) like ''%' +
179200170306                  %trim(D03UtC) +
179300170201                  '%''';
179400170201           endif;
179500170306           if D03Dati <> *blank;
179600170306             if D03Logica <> *blank
179700170306             or D03Lib <> *blank
179800170306             or D03ObjT <> *blank
179900170306             or D03Obj <> *blank
180000170306             or D03Attr <> *blank
180100170306             or D03UtC  <> *blank;
180200170306               wSQL = %trim(wSQL) +
180300170306               ' and';
180400170306             endif;
180500170306             wSQL = %trim(wSQL) +
180600170306                  ' FOBDati like ''%' +
180700170306                  %trim(D03Dati) +
180800170306                  '%''';
180900170306           endif;
181000170117         endif;
181100170117
181200170117       ENDSR;
181300170117
181400170117       //--------------------------------------------------------------
181500170117       //?Lettura occorrenza                                           ?
181600170117       //--------------------------------------------------------------
181700170117       BEGSR  Legge_SQL;
181800170117
181900161130         select;
182000161130
182100170201          // liv.1
182200170201          when wLivDrill = 1;
182300170201           exec sql  fetch next  from C1  into :WAFOB_fetch1;
182400161130         endsl;
182500160901
182600160901         *in99 = *off;
182700160901         if SQLCode = 100 or SQLCode < 0;
182800160901           *in99 = *on;
182900160901         endif;
183000160831
183100160831       ENDSR;
183200160831
183300170323       //--------------------------------------------------------------
183400170323       // Esecuzione del comando (già impostato).                      ?
183500170323       //--------------------------------------------------------------
183600170323       begsr  sr_ExecCmd;
183700170323
183800170323         clear Qcap0100;
183900170323         Qcabcsdh = *off;
184000170323         Qcapa    = *off;
184100170323         Qcacmdss = *off;
184200170323         Qcaerved = *allX'00';
184300170323
184400170323         clear Qusec;
184500170323         Qusbprv  = %size(Qusec);
184600170323
184700170323         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
184800170323                           %size(Qcap0100) : 'CPOP0100' : *omit :
184900170323                           0 : 0 : Qusec);
185000170323
185100170323         //if  Qusei <> *blank;
185200170323         //  ...;
185300170323         //endif;
185400170323
185500170323       endsr;
185600160831
185700970526     C*------------------------------------------------------------------------*
185800970526     C* NOTE COSTRUZIONE SCHIERE
185900970526     C*------------------------------------------------------------------------*
186000970526     C*
186100970526     C* -MSG-MESSAGGI
186200970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
186300970526     C* (NON TOCCARE MSG,1 E MSG,2)
186400970526     C*
186500970526     C* -CMD-COMANDI
186600970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
186700970526     C*
186800970526     C* -OPD-OPZIONI
186900970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
187000970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
187100970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
187200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
187300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
187400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
187500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
187600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
187700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
187800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
187900970526     C*
188000970526     C* -FUD-FUNZIONI
188100970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
188200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
188300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
188400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
188500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
188600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
188700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
188800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
188900970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
189000970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
189100970526     O*------------------------------------------------------------------------*
189200970526** DEC - DECODIFICHE
189300970526    GESTIONE                                                     1
189400970526INTERROGAZIONE                                                   2
189500970530     SCELTA                                                      3
1896009705264
1897009705265
189800970526** MSG - MESSAGGI
189900170201UBLGOBAR- Opzione non gestita                                                  1
190000170201UBLGOBAR- Tasto funzionale non gestito                                         2
190100170327UBLGOBAR- Cmd non eseguito:                                                    3
190200170201UBLGOBAR- Record annullato.                                                    4
190300170201UBLGOBAR- Record non annullato. Impossibile ripristinare                       5
190400170201UBLGOBAR- Accettato solo un parametro di inizio lista                          6
190500170201UBLGOBAR- Errori nei parametri di ingresso                                     7
190600170201UBLGOBAR- Data errata                                                          8
190700170201UBLGOBAR- Valore DA > valore AL                                                9
190800170201UBLGOBAR- Errore SQL nel reperimento dati Validi al                           10
190900170201UBLGOBAR- Copia totale (opz. 23) eseguita correttamente                       11
191000170201UBLGOBAR- Copia totale (opz. 23) non eseguita                                 12
191100170201UBLGOBAR- Non esiste un livello superiore per il DrillUp                      13
191200170201UBLGOBAR- Non esiste un livello inferiore per il DrillDown                    14
191300170327UBLGOBAR- ELABORAZIONE ESEGUITA                                               15
191400170201UBLGOBAR- 16                                                                  16
191500170201UBLGOBAR- 17                                                                  17
191600170201UBLGOBAR- 18                                                                  18
191700170201UBLGOBAR- 19                                                                  19
191800170201UBLGOBAR- 20                                                                  20
191900970526** CMD - COMANDI CL
192000161108
192100970526
192200970526
192300970526
192400970526
192500970526** OPD - OPZIONI
192600170323 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
192700170202 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
192800170327 3=Copia           -3 -     (S/N)=N                        opz. 03¬
192900170327 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
193000170301 5=Visualizzazione -5 -     (S/N)=S                        opz. 05¬
193100160713                   -  -     (S/N)=N                        opz. 06¬
193200160713                   -  -     (S/N)=N                        opz. 07¬
193300160713                   -  -     (S/N)=N                        opz. 08¬
193400160713                   -  -     (S/N)=N                        opz. 09¬
193500160713                   -  -     (S/N)=N                        opz. 10¬
19360017032711=Muovi           -11-     (S/N)=N                        opz. 11¬
193700970526                   -  -     (S/N)=N                        opz. 12¬
193800161108                   -  -     (S/N)=N                        opz. 13¬
19390017030214=Storicizzazione -14-     (S/N)=N                        opz. 14¬
194000970526                   -  -     (S/N)=N                        opz. 15¬
194100970526                   -  -     (S/N)=N                        opz. 16¬
194200970526                   -  -     (S/N)=N                        opz. 17¬
194300970526                   -  -     (S/N)=N                        opz. 18¬
194400970526                   -  -     (S/N)=N                        opz. 19¬
194500970526                   -  -     (S/N)=N                        opz. 20¬
194600970526                   -  -     (S/N)=N                        opz. 21¬
194700970526                   -  -     (S/N)=N                        opz. 22¬
194800161108                   -  -     (S/N)=N                        opz. 23¬
194900970526                   -  -     (S/N)=N                        opz. 24¬
195000970526** FUD - FUNZIONI
195100970526                   -        (S/N)=N                        funz.01¬
195200970526                   -        (S/N)=N                        funz.02¬
195300970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
195400970526                   -        (S/N)=N                        funz.04¬
195500970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
195600970526F06=Inserimento    -        (S/N)=N                        funz.06¬
195700170202F07=Drill-down     -        (S/N)=N                        funz.07¬
195800170202F08=Drill-up       -        (S/N)=N                        funz.08¬
195900170202                   -        (S/N)=N                        funz.09¬
196000160713                   -        (S/N)=N                        funz.10¬
196100160713                   -        (S/N)=N                        funz.11¬
196200160923F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
196300160713                   -        (S/N)=N                        funz.13¬
196400970603                   -        (S/N)=S                        funz.14¬
196500970526                   -        (S/N)=N                        funz.15¬
196600970526                   -        (S/N)=N                        funz.16¬
196700970526                   -        (S/N)=N                        funz.17¬
196800970526                   -        (S/N)=N                        funz.18¬
196900970627 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
197000970627 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
197100970526                   -        (S/N)=N                        funz.21¬
197200160713                   -        (S/N)=S                        funz.22¬
197300970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
197400970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
