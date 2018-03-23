000100000000      /TITLE ORM - Conferma e scrittura
000200060609
000300060609     H dftactgrp(*NO) actgrp(*CALLER) bnddir('TIO7':'TIS')
000400000000     f*--------------------------------------------------------------------------------------------*
000500000000     f*--------------------------------------------------------------------------------------------*
000600000000     f*---
000700000000     f* Tracciato libero ORM
000800000000     f*---
000900000000     Ftivpi00f  o    e             disk    USROPN
001000000000     f*---
001100000000     f* organigramma
001200000000     f*---
001300000000     Fazorg01l  IF   E           K DISK    USROPN
001400000000     f*---
001500000000     f* anagrafiche varie
001600000000     f*---
001700000000     Ftianv02l  IF   E           K DISK    USROPN
001800030702     F*
001900030702     F*---tio7100f  uf   e             disk    RENAME(tio7100f:tio71000)  USROPN
002000030702     F*---                                     PREFIX(f_)
002100060605
002200060609     D rtvMsgLang      PR          3512A                                        Messaggio in lingua
002300060612     D  msgId                         7A   CONST
002400060612     D  piLinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
002500060613     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
002600060613     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
002700060612     D                                     VARYING
002800060612     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
002900060612     D                                     VARYING
003000060612     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
003100060612     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
003200060612
003300060605     d*--------------------------------------------------------------------------------------------*
003400000000     d* Data structure
003500000000     d*--------------------------------------------------------------------------------------------*
003600040921     D                SDS
003700040921     D  PgmLibrary            81     90
003800030702     d*---
003900030702     d* Definizioni per la funzione TIO711R
004000030702     d*---
004100030702     D GetRifORM       PR            15
004200030702     D  ParametroU                   15
004300000000     d*---
004400000000     d* Variabili riferite al data base
004500000000     d*---
004600000000     d krgfil          S                   LIKE(orgfil)                         *lettura azorg01l
004700000000     d*---
004800000000     D* variabili di work
004900000000     d*---
005000040921     D CmdExcStr       S             50
005100040921     D CmdExcLen       S             15  5
005200040921     D                                     INZ(%SIZE(CmdExcStr))
005300030702     d  ormprg         S             15    inz(*all'0')
005400030702     D*-depprg         s                   like(ormprg)
005500030702     D*-wrkprg         s             12  0 inz(*zeros)
005600030702     D*-flgPRG         s              1    inz(*blanks)
005700000000     D  flgOK          s              1    inz(*blanks)
005800000000     D  wrkoggiiso     s               d
005900000000     D  wrkoggi        s              8  0
006000000000     D  wrkora         s              6  0
006100000000     D  datime         s             14
006200000000     D  wrkAlfaNum     s             10    inz(*all'0')
006300011023     d werr            S              1    INZ('0')                             *errore
006400011023     d $err            S              1    INZ('0')                             *controllore errore
006500000000     d wpgm            S             20                                         *programma
006600060605     D msgId...
006700060605     D                 S              7A
006800060605     D msgDta...
006900060605     D                 S            512A
007000060605     D msg1...
007100060605     D                 S            644A
007200060605     D secLvl...
007300060605     D                 S           3512A
007400060605     D rtnCode...
007500060605     D                 S             10A
007600060605     D esito15...
007700060605     D                 S             15P 0
007800061031     D wCodFiscPIva    s             16A
007900060605     d*---
008000000000     D* DS di procedura
008100000000     d*---
008200000000     D  tio110IDS    e ds                  prefix(Inp)
008300000000     D  tio110ODS    e ds                  prefix(Out)
008400000000     D  fnvaoaar     e ds                  prefix(W_)
008500050316     D DORM01        E DS
008600050316     D                                     INZ
008700120111     D fior96ds      E DS                  INZ QUALIFIED
008800120111     D kpjba         E DS                  INZ QUALIFIED
008900000000     d* controllo data
009000000000     d wlbda8          DS                  INZ                                  *controlla data
009100000000     d  g08dat                        8  0                                       -data dritta
009200000000     d  g08inv                        8  0                                       -data invertita
009300000000     d  g08err                        1                                          -errore
009400000000     d  g08tgi                        5  0                                       -giorni fra date
009500000000     d*---
009600000000     D* Messaggi di ritorno ad Internet
009700000000     d*---
009800011023     D  msg            s             80    dim(3) ctdata perrcd(1)
009900000000     C*--------------------------------------------------------------------------------------------*
010000000000     C* Main lines
010100000000     C*--------------------------------------------------------------------------------------------*
010200011017     c*
010300011017     c* operazioni iniziali -da fare sempre-
010400011017     C                   EXSR      Rutinz
010500011023     c*
010600011023     c* controlla input parte 1
010700011023     c                   EXSR      ChkParte1
010800000000     c*
010900000000     C* Calcola il progressivo ORM -Riferimento alfabetico-
011000011023if  1c                   IF        $err <> '1'                                  *no errore
011100000000     C                   EXSR      CalProg
011200000000     C*
011300000000     C* Effettua la valorizzazione/traduzione del FNVAO e la sua scrittura sul TIVPI
011400000000     C                   EXSR      WriVpi
011500011023e   1c                   ENDIF
011600000000     C*
011700000000     C                   SETON                                        lr
011800011023     c*--------------------------------------------------------------------------------------------*
011900011023     c* controlla input parte 1
012000011023     c*--------------------------------------------------------------------------------------------*
012100011023     c     ChkParte1     BEGSR
012200011023     c*---
012300011023     c* Può capitare che le persone si addormentino sopra le pagine e in tal modo i dati immessi,
012400011023     c* anche se controllati e corretti, non rimangano memorizzate perchè è passato troppo tempo.
012500011023     c* Occorre, quando si passa da una videata all'altra, RIcontrollare i dati immessi prima,
012600011023     c* testandone solamente l'obbligatorietà (se ci sono, sono corretti).
012700011023     c*
012800011023     c* La parte 2 non si controlla, perchè se manca la parte 2 manca anche la 1 e tornando indietro
012900011023     c* chiede obbligatoriamente i dati della parte 2.
013000011023     c*---
013100011023     c*
013200011023     c* reimposta le variabili di lavoro
013300011023     c                   MOVEL     '0'           werr                           *NO errore
013400011023     c*
013500011023     c* Cliente ritiro / Nominativo ritiro
013600011023if  1c                   IF        InpO110Cro <> *blanks OR
013700011023     c                             InpO110Rsr <> *blanks
013800011023x   1c                   ELSE
013900011023     c                   MOVEL     '1'           werr                           *errore Output
014000011023e   1c                   ENDIF
014100011023     c* Pagamemto
014200011023if  1c                   IF        InpO110Pag <> *blanks
014300011023x   1c                   ELSE
014400011023     c                   MOVEL     '1'           werr                           *errore Output
014500011023e   1c                   ENDIF
014600011023     c*
014700011023     c* se errore, esce dal pgm e non fa nient'altro
014800011023if  1c                   IF         werr = '1'
014900011023     c                   MOVEL     '1'           $err                           *controllore errore
015000011023     C                   eval      OutO110err = '1'
015100060605     C*                  eval      OutO110msg = msg(3)
015200060609     C                   EVAL      OutO110msg = rtvMsgLang('TIS0587'
015300060609     C                             :inpO110lang)
015400060609e   1c                   ENDIF
015500011023     c*
015600011023     c                   ENDSR
015700000000     C*--------------------------------------------------------------------------------------------*
015800000000      /TITLE Valorizzazione progressivo riferimento alfanumerico
015900000000     C*--------------------------------------------------------------------------------------------*
016000000000     C     CalProg       BEGSR
016100000000     C*
016200000000if  1C                   if        InpO110rfa = *blanks
016300030702     C*
016400030702     C*---               read(e)   tio7100f
016500030702if  2C*---               if        not %error
016600030702     C*---               eval      ormprg = f_tio7100f
016700030702     C*---               eval      depprg = f_tio7100f
016800030702     C*---
016900030702     C*---               move(p)   ormprg        wrkprg
017000030702     C*---               add       1             wrkprg
017100030702     C*---               move(p)   wrkprg        ormprg
017200030702     C*---               movel     'ORM'         ormprg
017300030702     C*---               eval      flgPRG = '1'
017400030702     C*---
017500030702e   2C*---               endif
017600030702     C*
017700030702     C                   eval      ormprg = GetRifORM(ormprg)
017800000000x   1C                   else
017900000000     C                   eval      ormprg = InpO110rfa
018000000000e   1C                   endif
018100000000     C*
018200000000     C                   ENDSR
018300000000     C*--------------------------------------------------------------------------------------------*
018400000000      /TITLE Scrittura TIVPI con struttura dati FNVAO
018500000000     C*--------------------------------------------------------------------------------------------*
018600000000     C     WriVpi        BEGSR
018700000000     C*
018800000000     C                   clear                   tivpi000
018900000000     C                   eval      vpitip = 'TO'
019000000000     C                   eval      vpiisv = 'TT'
019100000000     c                   if        InpO110ksu = *blanks
019200000000     C* reperisco il SUN dal file delle anagrafiche varie (tipo ORM)
019300000000     C     'ORM'         chain     tianv02l
019400000000     C                   if        %found(tianv02l)
019500000000     C                   eval      vpisun = anvcod
019600000000     c                   eval      vpiksu = anvcli
019700000000     C                   else
019800000000     C                   eval      vpisun = *zeros
019900000000     C                   eval      vpiksu = *zeros
020000000000     C                   endif
020100000000     c                   else
020200000000     C                   eval      vpiksu = InpO110ksu
020300000000     C                   movel(p)  InpO110rqscid vpisun
020400000000     c                   endif
020500000000     C*
020600000000     C                   exsr      wrivao
020700000000     C                   EVAL      vpidta = fnvaoaar
020800000000     C*
020900000000     C                   write(e)  tivpi000
021000000000     C                   if        not %error
021100040921     C                   eval      OutO110err = '0'
021200060605     C*                  eval      OutO110msg = msg(1)
021300060609     C                   EVAL      OutO110msg = rtvMsgLang('TIS0421'
021400060609     C                             :inpO110lang)
021500060609     C                   eval      OutO110rfa = ormprg
021600020320     C                   MOVEL     InpO110pe2    OutO110pe2                     *PO emissione
021700000000     c                   EXSR      DecodiPO                                     *Decodifica PO
021800030702     C*---               exsr      updprog
021900000000     C                   else
022000000000     C                   eval      OutO110err = '1'
022100060605     C*                  eval      OutO110msg = msg(2)
022200060609     C                   EVAL      OutO110msg = rtvMsgLang('TIS0173'
022300060609     C                             :inpO110lang)
022400060609     C                   eval      OutO110rfa = ormprg
022500000000     C                   endif
022600000000     C*
022700000000     C                   ENDSR
022800000000     C*--------------------------------------------------------------------------------------------*
022900000000      /TITLE Valorizzazione tracciato del FNVAO
023000000000     C*--------------------------------------------------------------------------------------------*
023100000000     C     WriVao        BEGSR
023200000000     C*
023300000000     C                   clear                   fnvaoaar
023400000000     C*
023500020320     C                   move(p)   InpO110pe2    W_vaopoe
023600000000if  1c                   IF        Inpo110Upd = '1'
023700000000     C                   eval      W_vaotor = 'S'
023800000000x   1c                   ELSE
023900000000     C                   eval      W_vaotor = 'M'
024000000000e   1c                   ENDIF
024100011109if  1c                   IF        Inpo110Ppg = 'S'
024200011109     C                   eval      W_vaotor = 'P'
024300011109e   1c                   ENDIF
024400000000     C                   eval      W_vaotco = 'I'
024500000000     C                   move(p)   wrkoggi       W_vaodao
024600000000     C                   move(p)   wrkora        W_vaooao
024700000000     C                   evalr     W_vaocor = %subst(InpO110ksu:2:7) + '000'
024800000000     C                   move(p)   InpO110cro    W_vaocra
024900000000     C                   eval      W_vaorsr = InpO110rsr
025000000000     C                   eval      W_vaoinr = InpO110inr
025100000000     C                   eval      W_vaocar = InpO110car
025200000000     C                   eval      W_vaolor = InpO110lor
025300000000     C                   eval      W_vaoprr = InpO110prr
025400000000     C                   eval      W_vaonar = InpO110nar
025500000000     C                   eval      W_vaorer = InpO110rer
025600000000     C                   eval      W_vaoter = InpO110ter
025700000000if  1c                   IF        Inpo110Upd = '1'
025800050208     C                   evalr     W_vaocrc = InpO110crc
025900000000     C                   eval      W_vaorsc = InpO110rsc
026000000000     C                   eval      W_vaoinc = InpO110inc
026100000000     C                   eval      W_vaocac = InpO110cac
026200000000     C                   eval      W_vaoloc = InpO110loc
026300000000     C                   eval      W_vaoprc = InpO110prc
026400000000     C                   eval      W_vaonac = InpO110nac
026500000000e   1c                   ENDIF
026600000000     C                   eval      W_vaoffd = InpO110ffd
026700000000     c*
026800000000     C*---               move(p)   InpO110dar    W_vaodar
026900000000     C                   MOVEL     InpO110dar    g08dat
027000000000     C                   Z-ADD     *zeros        g08inv
027100000000     C                   MOVEL     '0'           g08err
027200000000     c                   CALL      'XSRDA8'
027300000000     c                   PARM                    wlbda8
027400000000     c                   MOVE(P)   g08inv        W_vaodar
027500000000     c*
027600000000     C                   move(p)   InpO110orr    W_vaoorr
027700000000     C                   eval      W_vaonam = InpO110nam
027800000000     C                   move(p)   InpO110ncl    W_vaoncl
027900000000     C                   movel(p)  InpO110pkg    wrkAlfaNum
028000000000     C                   evalr     W_vaopkg = %subst(wrkAlfaNum:1:6)+','+
028100000000     C                                        %subst(wrkAlfaNum:7:1)
028200000000     C                   movel(p)  InpO110vlm    wrkAlfaNum
028300000000     C                   evalr     W_vaovlm = %subst(wrkAlfaNum:1:2)+','+
028400000000     C                                        %subst(wrkAlfaNum:3:3)
028500000000     C                   move(p)   InpO110bnc    W_vaobnc
028600000000     C                   move(p)   InpO110blc    W_vaoblc
028700000000     C                   move(p)   InpO110att    W_vaoatt
028800000000     C                   move(p)   InpO110mtc    W_vaomtc
028900000000     C                   eval      W_vaopag = InpO110pag
029000000000     C                   move(p)   InpO110pr2    W_vaopor
029100120111     ** Forzatura della filiale ritiro.
029200120111     C                   RESET                   fior96ds
029300120111     C                   MONITOR
029400120111     C                   EVAL      fior96ds.ior96Poe = %INT(w_vaoPoe)
029500120111     C                   EVAL      fior96ds.ior96Por = %INT(w_vaoPor)
029600120111     C                   CALL      'FIOR96R'
029700120111     C                   PARM                    kpjba
029800120111     C                   PARM                    fior96ds
029900120111     C                   IF        fior96ds.oor96Err = *BLANK AND
030000120111     C                             fior96ds.oor96Por > *ZERO
030100120111     C                   EVALR     w_vaoPor = %EDITC(fior96ds.oor96Por:'X')
030200120111     C                   ENDIF
030300120111     C                   ON-ERROR
030400120111     C                   ENDMON
030500120111     C
030600000000     C                   move(p)   InpO110poc    W_vaopoc
030700000000     c*
030800000000     C                   eval      W_vaono1 = InpO110no1
030900000000     C                   eval      W_vaono2 = InpO110no2
031000000000     c*
031100000000     C                   eval      W_vaorfa = ormprg
031200050323     **
031300050316     ** Contatto telefonico preventivo.
031400050323     ** il valore del flag sul file deve essere: *Blk/'N'
031500050316     C                   RESET                   DORM01
031600050323     **  quindi se riceve "S" lo converte
031700091009     c*                  if        InpO110CttTlf = 'S'
031800091009     c*                  eval      §ORCOMC = *blank
031900091009     c*                  else
032000050316     C                   EVAL      §ORCOMC = InpO110CttTlf
032100091009     c*                  end
032200050323      *
032300050316     C                   eval      W_VAOFLO = DORM01
032400061031     C*
032500061031     C* Effettuo considerazioni in caso di accesso anonimo x recepire i dati relativi a
032600061031     C* P.IVA e/o Cod.Fisc.
032700061031     C                   if        InpO110ksu = *blanks
032800061031     C                   clear                   wCodFiscPIva
032900061031     C* Precenza al Cod.Fisc.
033000061031     C                   if        InpO110CFO <> *blanks
033100061031     C                   eval      wCodFiscPIva = %trim(InpO110CFO)
033200061031     C* Altrimenti P.IVA
033300061031     C                   else
033400061031     C                   eval      wCodFiscPIva = %trim(InpO110PIO)
033500061031     C                   endif
033600061031     C                   move      wCodFiscPIva  W_VAOINO
033700061031     C                   endif
033800000000     C*
033900000000     C                   ENDSR
034000000000     C*--------------------------------------------------------------------------------------------*
034100000000      /TITLE Aggiornamento progressivo riferimento alfanumerico
034200000000     C*--------------------------------------------------------------------------------------------*
034300030702     C*--- UpdProg       BEGSR
034400030702     C*---
034500030702if  1C*---               if        flgPRG = '1'
034600030702     C*---               eval      f_tio7100f = ormprg
034700030702     C*---               update    tio71000
034800030702e   1C*---               endif
034900030702     C*---
035000030702     C*---               ENDSR
035100000000     C*--------------------------------------------------------------------------------------------*
035200000000      /TITLE Decodofica punto operativo di ritiro
035300000000     C*--------------------------------------------------------------------------------------------*
035400000000     C     DecodiPO      BEGSR
035500000000     C*
035600020320     c                   MOVEL     OutO110pe2    krgfil
035700000000     c     keyorg        CHAIN     azorg01l                           99
035800000000if  1c                   IF        NOT *in99
035900000000     C                   EVAL      OutO110Des = orgDes
036000000000     C                   EVAL      OutO110Ind = orgInd
036100000000     C                   MOVEL     ORGCPF        OutO110Cpf
036200000000     C                   EVAL      OutO110Loc = orgLoc
036300000000     C                   EVAL      OutO110Pro = orgPro
036400000000     C                   EVAL      OutO110Tel = orgTel
036500000000     C                   EVAL      OutO110Fax = orgFax
036600000000e   1c                   ENDIF
036700000000     C*
036800000000     C                   ENDSR
036900011017     C*--------------------------------------------------------------------------------------------*
037000011017      /TITLE Operazioni iniziali -da fare sempre-
037100011017     C*--------------------------------------------------------------------------------------------*
037200011017     C     rutinz        BEGSR
037300011017     C*
037400011017     C* azzera le variabili di lavoro
037500011017     C                   CLEAR                   tio110ods
037600011023     c                   MOVEL     '0'           $err                           *controllore errore
037700011017     C*
037800011017     C                   ENDSR
037900000000     C*--------------------------------------------------------------------------------------------*
038000000000      /TITLE Operazioni iniziali
038100000000     C*--------------------------------------------------------------------------------------------*
038200000000     C     *inzsr        BEGSR
038300000000     C*
038400000000     C* Ricevimento parametri da AS777 via "CONNECT SQL"
038500000000     C     *entry        plist
038600000000     C                   parm                    TIO110IDS
038700000000     C                   parm                    TIO110ODS
038800000000     c*
038900000000     c* chiavi di lettura
039000000000     c     keyorg        KLIST                                                  azorg01l
039100000000     c                   KFLD                    krgfil                         -filiale
039200000000     C*
039300000000     C                   time                    wrkoggiiso
039400000000     C                   time                    wrkora
039500000000     C     *iso          move      wrkoggiiso    wrkoggi
039600000000     C                   move(p)   wrkora        datime
039700000000     C                   movel     wrkoggi       datime
039800040921
039900040921     **?Aggiungo alla lista librerie la libreria dove risiede il programma.
040000060605     C*                  EVAL      CmdExcStr = 'ADDLIBLE'
040100060605     C*                            + ' LIB(' + %TRIMR(PgmLibrary) + ')'
040200060605     C*                            + ' POSITION(*AFTER QTEMP)'
040300060605     C*                  CALL(E)   'QCMDEXC'
040400060605     C*                  PARM                    CmdExcStr
040500060605     C*                  PARM                    CmdExcLen
040600040921
040700000000     c*
040800000000     c* aggiunge le librerie in lista
040900060605     c*                  EVAL      wpgm = 'GAITRAOBJ/TIS777C'
041000060605     c*                  CALL      wpgm
041100000000     c*
041200000000     c* apre i files
041300000000     c                   OPEN      tivpi00f
041400030702     c*---               OPEN      tio7100f
041500000000     c                   OPEN      azorg01l
041600000000     c                   OPEN      tianv02l
041700000000     C*
041800000000     C                   ENDSR
041900000000     O*--------------------------------------------------------------------------------------------*
042000060605
042100000000** MSG ***
042200000000Operazione eseguita con successo, richiesta ritiro merce inoltrata.
042300000000Errori nell'operazione, richiesta ritiro merce non inoltrata.
042400011023Tornare indietro e RIcontrollare i dati immessi
