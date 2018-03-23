000100010813      /TITLE - T&T: Immissione disposizioni di giacenza
000200060609     H DFTACTGRP(*NO) ACTGRP('WRTTIVPITR') BNDDIR('QC2LE':'QUSAPIBD':'TIS')
000300050921
000400010813     f*--------------------------------------------------------------------------------------------*
000500010813     f* Data base
000600010813     f*--------------------------------------------------------------------------------------------*
000700010813     Ftivpi00f  O    E             DISK    USROPN
000800051108     F                                     COMMIT
000900010813     Fazorg01l  IF   E           K DISK    USROPN
001000050921
001100050921     ***********************************************************************************************
001200050921     **?
001300050921     **?Definizione procedure.
001400050921     **?
001500050921     ***********************************************************************************************
001600050922     D atoi            PR            10I 0                                      String to integer
001700050921     D                                     EXTPROC('atoi')
001800050922     D  a                              *
001900050921     D                                     VALUE
002000050921     D                                     OPTIONS(*STRING)
002100050922     D QlgConvertCase  PR
002200050922     D                                     EXTPROC('QlgConvertCase')
002300050922     D  a                            22                                         Request control bloc
002400050922     D  a                            50                                         Input data
002500050922     D  a                            50                                         Output data
002600050922     D  i                            10I 0                                      Length of data
002700050922     D                                     CONST
002800050922     D  a                            16                                         Error code
002900051108     D pCmd            PR                                                       Process Commands
003000051108     D                                     EXTPGM('QCAPCMD')
003100051108     D  pCmd01                      128                                         Source command
003200051108     D                                                                          string
003300051108     D  pCmd02                       10I 0                                      Length of source
003400051108     D                                                                          command string
003500051108     D  pCmd03                                                                  Options control
003600051108     D                                     LIKE(qcap0100)                       block
003700051108     D  pCmd04                       10I 0                                      Options control
003800051108     D                                                                          block length
003900051108     D  pCmd05                        8                                         Options control
004000051108     D                                                                          block format
004100051108     D  pCmd06                                                                  Changed command
004200051108     D                                     LIKE(PCmd01)                         string
004300051108     D  pCmd07                       10I 0                                      Length available for
004400051108     D                                                                          changed command
004500051108     D                                                                          string
004600051108     D  pCmd08                       10I 0                                      Length of changed
004700051108     D                                                                          command string
004800051108     D                                                                          available to return
004900051108     D  pCmd09                                                                  Error Code
005000051108     D                                     LIKE(qusec)
005100051109     D tis7653r        PR
005200051110     D                                     EXTPGM('TIS7653R')
005300051109     D  ksu
005400051109     D                                     LIKE(vpiKsu)
005500051109     D  agc
005600051109     D                                     LIKE(diz$aa4)
005700051109     D  fgc
005800051109     D                                     LIKE(diz$fil)
005900051109     D  ngc
006000051109     D                                     LIKE(diz$ngc)
006100051109     D  esito10                      10I 0
006200060609     D rtvMsgLang      PR          3512A                                        Messaggio in lingua
006300060612     D  msgId                         7A   CONST
006400060612     D  piLinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
006500060613     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
006600060613     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
006700060612     D                                     VARYING
006800060612     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
006900060612     D                                     VARYING
007000060612     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
007100060612     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
007200060612
007300050921     ***********************************************************************************************
007400050921     **?
007500050921     **?Definizione costanti.
007600050921     **?
007700050921     ***********************************************************************************************
007701170328
007702170328      /COPY GAITRASRC/SRCCONST,FNLGW00R
007800050908     D StessoIndirizzo...
007900050908     D                 C                   '1'
008000050908     D AltroIndirizzo  C                   '2'
008100050908     D FermoDeposito   C                   '3'
008200050908     D Rendere         C                   '4'
008300050908     D Altre           C                   '5'
008400050908     D AppenaPossibile...
008500050908     D                 C                   '1'
008600051014     D InData          C                   '2'
008700050908     D DopoIl          C                   '3'
008800050908     D Appuntamento    C                   '4'
008900050922     D ToUpperCase     C                   0
009000051014     **?Se si modificano le seguenti costanti l'immissione automatica delle disposizioni di
009100051014     **?giacenza non riconoscerà più le disposizioni arrivate dal sito VAS.
009200051014     D TestoStessoIndirizzo...
009300051014     D                 C                   'RICONSEGNARE STESSO INDIRIZZO'
009400051014     D TestoAppuntamento...
009500051014     D                 C                   'RICONSEGNARE STESSO INDIRIZZO -
009600051014     D                                     CON APPUNTAMENTO'
009700051014     D TestoAltroIndirizzo...
009800051014     D                 C                   'RICONSEGNARE ALTRO INDIRIZZO'
009900051014     D TestoFermoDeposito...
010000051014     D                 C                   'RICONSEGNARE - F.D.'
010100051014     D TestoRendere...
010200051014     D                 C                   'RENDERE'
010300050921
010400050921     ***********************************************************************************************
010500050921     **?
010600050921     **?Definizione strutture dati.
010700050921     **?
010800050921     ***********************************************************************************************
010900051109     D cnDizion      E DS
011000051109     D                                     BASED(nullPtr)
011100051109     D                                     PREFIX(diz)
011200050922      /COPY QSYSINC/QRPGLESRC,QUSEC
011300051108      /COPY QSYSINC/QRPGLESRC,QCAPCMD
011400050922     D lgCvtCase1      DS                                                       Request control bloc
011500050922     D                                     INZ
011600050922     D  lgCvtCase1a                  10I 0                                      Type of request
011700050922     D                                     INZ(1)                               CCSID format
011800050922     D  lgCvtCase1b                  10I 0                                      CCSID of input data
011900050922     D  lgCvtCase1c                  10I 0                                      Case request
012000050922     D                                     INZ(ToUpperCase)
012100050922     D  lgCvtCase1d                  10                                         Reserved
012200050922     D                                     INZ(X'00000000000000000000')
012300050908     D dgnpdmc912    E DS
012400050908     D                                     PREFIX(dmc912)
012500050908     D dgnpdmc913    E DS
012600050908     D                                     PREFIX(dmc913)
012700050908     D dgnpdmc915    E DS
012800050908     D                                     PREFIX(dmc915)
012900050908     D dgnpdmc917    E DS
013000050908     D                                     PREFIX(dmc917)
013100050908     D dgnpdmc924    E DS
013200050908     D                                     PREFIX(dmc924)
013300010813      *---
013400010813      * Variabili riferite al data base
013500010813      *---
013600010813     d krgfil          S                   LIKE(orgfil)                         *lettura azorg01l
013700010813      *---
013800010813      * variabili di lavoro
013900010813      *---
014000050831     D rqsDta          S           9999
014100050831     D rpyDta          S           9999
014200050901     D esito           S              5I 0
014300010813     d n3              S              3  0                                      *numerico di 3
014400010813     d n4              S              4  0                                      *numerico di 4
014500010813     d a50             S             50                                         *alfabetico di 50
014600010813     d wrkoggiiso      S               D
014700010813     d wrkoggi         S              8  0                                      *data corrente
014800010813     d wrkora          S              6  0                                      *ora  corrente
014900010813     d datime          S             14                                         *data + ora
015000010813     d $ErrWrite       S              1    INZ('N')                             *errore in scrittura
015100050901     D wrkDmc          S
015200050901     D                                     LIKE(w_vhrDmc)
015300050901     D                                     DIM(5)
015400050901     D i               S              5I 0
015500051108     D pCmd01          S            128
015600051108     D pCmd02          S             10I 0
015700051108     D                                     INZ(%SIZE(PCmd01))
015800051108     D pCmd04          S             10I 0
015900051108     D                                     INZ(%SIZE(qcap0100))
016000051108     D pCmd05          S              8
016100051108     D                                     INZ('CPOP0100')
016200051108     D pCmd06          S
016300051108     D                                     LIKE(PCmd01)
016400051108     D pCmd07          S             10I 0
016500051108     D                                     INZ(%SIZE(PCmd06))
016600051108     D pCmd08          S             10I 0
016700051109     D nullPtr         S               *
016800051109     D agc             S
016900051109     D                                     LIKE(diz$aa4)
017000051109     D fgc             S
017100051109     D                                     LIKE(diz$fil)
017200051109     D ngc             S
017300051109     D                                     LIKE(diz$ngc)
017400051109     D esito10         S             10I 0
017500060606     D msgId...
017600060606     D                 S              7A
017700060606     D msgDta...
017800060606     D                 S            512A
017900060606     D msg1...
018000060606     D                 S            644A
018100060606     D secLvl...
018200060606     D                 S           3512A
018300060606     D rtnCode...
018400060606     D                 S             10A
018500060606     D esito15...
018600060606     D                 S             15P 0
018700060606      *---
018800010813      * DS
018900010813      *---
019000010813      * di procedura                                                            *Input
019100050831     D Tis1650dsi    E DS                  PREFIX(Inp)                          *Output
019101170322     D                                     EXTNAME('STRATEDEV/TIS1650DSI')
019200170316     D Tis1650dso    E DS                  PREFIX(Out)
019201170316     D                                     EXTNAME('STRATEDEV/TIS1650DSO')
019300010813      * di scrittura file di testo                                              *Input
019400170322     D fnvareiir     E DS                  PREFIX(W_)
019401170316     D trulDscDs     E DS                  QUALIFIED INZ
019500010813      *---
019600010813      * Messaggi di ritorno ad Internet
019700010813      *---
019800010813     D  msg            s             80    DIM(2) CTDATA PERRCD(1)
019900050921
020000010813     C*--------------------------------------------------------------------------------------------*
020100010813     C* Main lines
020200010813     C*--------------------------------------------------------------------------------------------*
020300050921     C                   CLEAR                   Tis1650dso
020400050901     C*
020500050921     C                   EXSR      chkFase
020600050921     c                   EXSR      DecodiPO                                     *Decodifica PO
020700050921     C*
020800050921     C                   IF        outErr <> *ON
020900050901     c* reperimento data / ora corrente
021000050901     C                   TIME                    wrkoggiiso
021100050901     C                   TIME                    wrkora
021200050901     C     *ISO          MOVE      wrkoggiiso    wrkoggi                        data corrente aa/m/g
021300050901     C                   MOVE(P)   wrkora        datime                         ora  corrente
021400050901     c*
021500050901     c* apre i files
021600050901     c                   OPEN      tivpi00f
021700010813     C*
021800010813     C* Effettua la valorizzazione/traduzione del FNVAR e la sua scrittura sul TIVPI
021900010813     C                   EXSR      WriVpi
022000050901     c*
022100050901     c                   CLOSE     tivpi00f
022200050921     C*
022300050921     C                   ENDIF
022400010813     C*
022500050901     C                   RETURN
022600010813     C*--------------------------------------------------------------------------------------------*
022700010813      /TITLE Scrittura TIVPI con struttura dati FNVAR
022800010813     C*--------------------------------------------------------------------------------------------*
022900010813     C     WriVpi        BEGSR
023000010813     C*
023100010813     C* azzera variabili di lavoro
023200010813     C                   CLEAR                   Tivpi000                       *file testo
023300010813     C                   RESET                   $ErrWrite                      *Errori in write
023400050901     C                   CLEAR                   wrkDmc
023500050901     C                   CLEAR                   i
023600010831     c* scrittura
023700010813     C                   EVAL      vpitip = 'TR'                                *tipo file
023800010831     C                   EVAL      vpiisv = 'TT'                                *tipo servizio
023900050831     C                   MOVEL     InpSun        vpisun                         *ID Strategi
024000050831     C                   EVAL      vpiksu = Inpksu                              *cliente unificante
024100010813     C*
024200010813     c* valorizzazione tracciato del FNVAR e scrittura TIVPI
024300010813     C                   EXSR      WriVar
024400010813     c* NO errore
024500010831if  1C                   IF        $ErrWrite = 'N'                              *NO errore
024600051108     C                   COMMIT
024700050831     C                   EVAL      OutErr = '0'
024800060606     C*                  EVAL      OutMsg = msg(1)
024900060609     C                   EVAL      OutMsg = rtvMsgLang('TIS0420'
025000060609     C                             :inplang)
025100060609     C                   EVAL      outDmc1 = wrkDmc(1)
025200050901     C                   EVAL      outDmc2 = wrkDmc(2)
025300050901     C                   EVAL      outDmc3 = wrkDmc(3)
025400050901     C                   EVAL      outDmc4 = wrkDmc(4)
025500050901     C                   EVAL      outDmc5 = wrkDmc(5)
025600010813     c* SI errore
025700010831x   1C                   ELSE                                                   *SI errore
025800051108     C                   ROLBK
025900050831     C                   EVAL      OutErr = '1'
026000060606     C*                  EVAL      OutMsg = msg(2)
026100060609     C                   EVAL      OutMsg = rtvMsgLang('TIS0172'
026200060609     C                             :inplang)
026300060609e   1C                   ENDIF
026400010813     C*
026500010813     C                   ENDSR
026600050901
026700010813     C*--------------------------------------------------------------------------------------------*
026800010813      /TITLE Valorizzazione tracciato del FNVAR
026900010813     C*--------------------------------------------------------------------------------------------*
027000010813     C     WriVar        BEGSR
027100010813     C*
027200010813     C* azzera variabili di lavoro
027300170322     C                   CLEAR                   fnvareiir
027400010813     C                   RESET                   $ErrWrite                      *Errori in write
027500010813     C* imposta campi
027600050831     C                   MOVEL     InpDsp        n4
027700010813     C                   MOVE(P)   n4            W_vhraas                       *anno   spedizione
027800050831     C                   MOVE(P)   InpLnp        W_vhrlnp                       *linea  spedizione
027900050831     C                   MOVE(P)   InpNrs        W_vhrnrs                       *serie  spedizione
028000050831     C                   MOVE(P)   InpNsp        W_vhrnsp                       *numero spedizione
028100050831     C                   MOVE      InpDag        n4
028200010813     C                   MOVE(P)   n4            W_vhragc                       *anno     giacenza
028300050831     C                   MOVE(P)   InpFgc        W_vhrfgc                       *filiale  giacenza
028400050831     C                   MOVE(P)   InpNgc        W_vhrngc                       *numero   giacenza
028500050831     C                   MOVE(P)   InpCcm        W_vhrccm                       *mittente giacenza
028600010813     C                   MOVE(P)   wrkoggi       W_vhrdtr                       *data trasmissione
028700010813     C                   MOVE(P)   *zeros        W_vhrdrd                       *data ricezione
028701170328      // E' possibile inserire le disposizioni di giacenza in 3 modi:
028702170328      // "C" = login sul sito VAS.
028703170328      // "B" = inserendo il BRTcode.
028705170328      // "K" = ricevendo email con URL contenente la chiave giacenza KEYGIAC.
028706170328      // "W" = Nulla/altro (non dovrebbe mai accadere).
028707170322     C                   SELECT
028708170322     C                   WHEN      inpSun <> *ZERO
028709170328     C                   EVAL      w_vhrdmcta = FNLGW00_TIPO_ACCESSO_CODIFICATO
028710170322     C                   WHEN      inpBrtCode <> *BLANK
028711170328     C                   EVAL      w_vhrdmcta = FNLGW00_TIPO_ACCESSO_BRTCODE
028712170328     C                   WHEN      inpKeyGiac <> *BLANK
028713170328     C                   EVAL      w_vhrdmcta = FNLGW00_TIPO_ACCESSO_KEYGIAC
028714170322     C                   OTHER
028715170328     C                   EVAL      w_vhrdmcta = FNLGW00_TIPO_ACCESSO_ANONIMO
028716170322     C                   ENDSL
028800010813     c*
028900010813     c                   MOVEL     *zeros        W_vhrnpr                       *prgressivo note
029000010813     c                   Z-ADD     *zeros        n3
029100050901
029200050901     C                   SELECT
029300050901     C                   WHEN      inpTpDisposiz = StessoIndirizzo
029400050901     C                   EXSR      setStessoInd
029500050901     C                   WHEN      inpTpDisposiz = AltroIndirizzo
029600050901     C                   EXSR      setAltroInd
029700050901     C                   WHEN      inpTpDisposiz = FermoDeposito
029800050901     C                   EXSR      setFermoDepos
029900050901     C                   WHEN      inpTpDisposiz = Rendere
030000050901     C                   EXSR      setRendere
030100050901     C                   WHEN      inpTpDisposiz = Altre
030200050901     C                   EXSR      setAltre
030300050901     C                   OTHER
030400050901     C                   EVAL      $ErrWrite = 'S'
030500050901     C                   ENDSL
030600050901
030700050901     C                   ENDSR
030800050901
030900050901     ***********************************************************************************************
031000050901     **?
031100050901     **?Impostazione note consegna stesso indirizzo.
031200050901     **?
031300050901     ***********************************************************************************************
031400050901     C     setStessoInd  BEGSR
031500050901
031600050901     C                   SELECT
031700050901     C
031800050901     C                   WHEN      inpStessoInd = AppenaPossibile
031900050901     C                   EVALR     w_vhrNpr = '910'
032000051014     C                   EVAL      w_vhrDmc = TestoStessoIndirizzo
032100050901     C                   EXSR      wrtTivpi
032200050901     C
032300051014     C                   WHEN      inpStessoInd = InData OR
032400051014     C                             inpStessoInd = DopoIl
032500050901     C                   EVALR     w_vhrNpr = '914'
032600051014     C                   EVAL      w_vhrDmc = TestoStessoIndirizzo
032700050901     C                   EXSR      wrtTivpi
032800050901     C                   EVALR     w_vhrNpr = '915'
032900050908     C                   CLEAR                   dgnpdmc915
033000050901     C                   SELECT
033100051014     C                   WHEN      inpStessoInd = InData
033200051014     C                   EVAL      dmc915costante = 'IN DATA'
033300050901     C                   WHEN      inpStessoInd = DopoIl
033400051014     C                   EVAL      dmc915costante = 'DOPO IL'
033500050901     C                   ENDSL
033600050908     C     *EUR          MOVE      inpNdcr       dmc915ndcr
033700050908     C                   EVAL      w_vhrDmc = dgnpdmc915
033800050901     C                   EXSR      wrtTivpi
033900050901     C
034000050901     C                   WHEN      inpStessoInd = Appuntamento
034100050901     C                   EVALR     w_vhrNpr = '911'
034200051014     C                   EVAL      w_vhrDmc = TestoAppuntamento
034300050901     C                   EXSR      wrtTivpi
034400050901     C                   EVALR     w_vhrNpr = '912'
034500050908     C                   CLEAR                   dgnpdmc912
034600051014     C                   EVAL      dmc912costante = 'TELEFONO'
034700050908     C                   EVAL      dmc912telefono = inpAptTelefon
034800050908     C                   EVAL      w_vhrDmc = dgnpdmc912
034900050901     C                   EXSR      wrtTivpi
035000050901     C                   EVALR     w_vhrNpr = '913'
035100050908     C                   CLEAR                   dgnpdmc913
035200051014     C                   EVAL      dmc913costante = 'SIG.'
035300050908     C                   EVAL      dmc913persona = inpAptPersona
035400050908     C                   EVAL      w_vhrDmc = dgnpdmc913
035500050901     C                   EXSR      wrtTivpi
035600050901     C
035700050901     C                   OTHER
035800050901     C                   EVAL      $ErrWrite = 'S'
035900050901     C                   ENDSL
036000050901
036100050901     C                   ENDSR
036200050901
036300050901     ***********************************************************************************************
036400050901     **?
036500050901     **?Impostazione note consegna ad altro indirizzo.
036600050901     **?
036700050901     ***********************************************************************************************
036800050901     C     setAltroInd   BEGSR
036900050901
037000050901     C                   EVALR     w_vhrNpr = '920'
037100051014     C                   EVAL      w_vhrDmc = TestoAltroIndirizzo
037200050901     C                   EXSR      wrtTivpi
037300050901     C                   EVALR     w_vhrNpr = '921'
037400050901     C                   EVAL      w_vhrDmc = inpAinRagSoc1
037500050901     C                   EXSR      wrtTivpi
037600050927     C*                  IF        inpAinRagSoc2 <> *BLANK
037700050901     C                   EVALR     w_vhrNpr = '922'
037800050901     C                   EVAL      w_vhrDmc = inpAinRagSoc2
037900050901     C                   EXSR      wrtTivpi
038000050927     C*                  ENDIF
038100050901     C                   EVALR     w_vhrNpr = '923'
038200050901     C                   EVAL      w_vhrDmc = inpAinIndiriz
038300050901     C                   EXSR      wrtTivpi
038400050901     C                   EVALR     w_vhrNpr = '924'
038500050908     C                   CLEAR                   dgnpdmc924
038600050908     C                   EVAL      dmc924cap = inpAinCap
038700050908     C                   EVAL      dmc924loc = inpAinLocalit
038800050908     C                   EVAL      dmc924pro = inpAinProvinc
038900050908     C                   EVAL      dmc924naz = inpAinNazione
039000050908     C                   EVAL      w_vhrDmc = dgnpdmc924
039100050901     C                   EXSR      wrtTivpi
039200050901
039300050901     C                   ENDSR
039400050901
039500050901     ***********************************************************************************************
039600050901     **?
039700050901     **?Impostazione note fermo deposito.
039800050901     **?
039900050901     ***********************************************************************************************
040000050901     C     setFermoDepos BEGSR
040100050901
040200050901     C                   EVALR     w_vhrNpr = '916'
040300051014     C                   EVAL      w_vhrDmc = TestoFermoDeposito
040400050901     C                   EXSR      wrtTivpi
040500050901     C                   EVALR     w_vhrNpr = '917'
040600050908     C                   CLEAR                   dgnpdmc917
040700051014     C                   EVAL      dmc917costante = 'RITIRERÀ'
040800050908     C                   EVAL      dmc917persona = %TRIML(inpRfdPersona)
040900050908     C                   EVAL      w_vhrDmc = dgnpdmc917
041000050901     C                   EXSR      wrtTivpi
041100050901
041200050901     C                   ENDSR
041300050901
041400050901     ***********************************************************************************************
041500050901     **?
041600050901     **?Impostazione note rendere al mittente.
041700050901     **?
041800050901     ***********************************************************************************************
041900050901     C     setRendere    BEGSR
042000050901
042100050901     C                   EVALR     w_vhrNpr = '900'
042200051014     C                   EVAL      w_vhrDmc = TestoRendere
042300050901     C                   EXSR      wrtTivpi
042400050901
042500050901     C                   ENDSR
042600050901
042700050901     ***********************************************************************************************
042800010813     c* scrive tanti record quanto lunghe sono le note immesse (200 : 50 = max 4 record)
042900010813     c* e da esito OK solo se TUTTE le scritture sono andate a buon fine
043000050901     ***********************************************************************************************
043100050901     C     setAltre      BEGSR
043200050901
043300050831     C     x'0D':' '     XLATE     InpDmc        InpDmc                         *toglie "A CAPO"
043400050831     C     x'25':' '     XLATE     InpDmc        InpDmc                         *toglie "A CAPO"
043500050831     C                   EVAL      InpDmc = %TRIM(InpDmc)                       *toglie spazi Dx/Sx
043600010813     c*
043700050831     C                   EVAL      a50 = %subst(InpDmc:1:50)
043800010813if  1c                   IF        a50 <> *blanks
043900010813     c                   ADD       1             n3
044000010813     c                   MOVE(P)   n3            W_vhrnpr                       *progressivo nota
044100010813     c                   EVAL      W_vhrdmc = a50                               *nota
044200050902     C                   EXSR      wrtTivpi
044300010813e   1c                   ENDIF
044400010813     c*
044500010813if  1C                   IF        $ErrWrite = 'N'                              *NON ancora errori
044600050831     C                   EVAL      a50 = %subst(InpDmc:51:50)                   *SECONDA
044700010813if  2c                   IF        a50 <> *blanks
044800010813     c                   ADD       1             n3
044900010813     c                   MOVE(P)   n3            W_vhrnpr                       *progressivo nota
045000010813     c                   EVAL      W_vhrdmc = a50                               *nota
045100050902     C                   EXSR      wrtTivpi
045200010813e   2c                   ENDIF
045300010813e   1c                   ENDIF
045400010813     c*
045500010813if  1C                   IF        $ErrWrite = 'N'                              *NON ancora errori
045600050831     C                   EVAL      a50 = %subst(InpDmc:101:50)                  *TERZA
045700010813if  2c                   IF        a50 <> *blanks
045800010813     c                   ADD       1             n3
045900010813     c                   MOVE(P)   n3            W_vhrnpr                       *progressivo nota
046000010813     c                   EVAL      W_vhrdmc = a50                               *nota
046100050902     C                   EXSR      wrtTivpi
046200010813e   2c                   ENDIF
046300010813e   1c                   ENDIF
046400010813     c*
046500010813if  1C                   IF        $ErrWrite = 'N'                              *NON ancora errori
046600080128     C                   EVAL      a50 = %subst(InpDmc:151:50)                  *QUARTA
046700010813if  2c                   IF        a50 <> *blanks
046800010813     c                   ADD       1             n3
046900010813     c                   MOVE(P)   n3            W_vhrnpr                       *progressivo nota
047000010813     c                   EVAL      W_vhrdmc = a50                               *nota
047100050902     C                   EXSR      wrtTivpi
047200010813e   2c                   ENDIF
047300010813e   1c                   ENDIF
047400010813     c*
047500010813     C                   ENDSR
047600050901
047700050901     ***********************************************************************************************
047800050901     **?
047900050901     **?Scrittura TIVPI00F.
048000050901     **?
048100050901     ***********************************************************************************************
048200050901     C     wrtTivpi      BEGSR
048300050901
048400050922     C                   CLEAR                   qusec
048500050922     C                   EVAL      qusbprv = %SIZE(qusec)
048600050922     C                   CALLP     QlgConvertCase(lgCvtCase1:w_vhrDmc:w_vhrDmc:
048700050922     C                             %SIZE(w_vhrDmc):qusec)
048800170322     C                   EVAL      vpiDta = fnvareiir
048900061106     C                   IF        inpSun <> 39368                              No demo Bartolini
049000050901     C                   WRITE(E)  tivpi000
049100061106e   3C                   ENDIF
049200050901if  3C                   IF        %ERROR
049300050901     C                   EVAL      $ErrWrite = 'S'
049400050901     C                   ELSE
049500050901     C                   EVAL      i = i + 1
049600050901     C                   EVAL      wrkDmc(i) = w_vhrDmc
049700050901e   3C                   ENDIF
049800050901
049900050901     C                   ENDSR
050000050901
050100010813     C*--------------------------------------------------------------------------------------------*
050200010813      /TITLE Decodofica punto operativo che gestirà le disposizioni
050300010813     C*--------------------------------------------------------------------------------------------*
050400010813     C     DecodiPO      BEGSR
050500010813     C*
050600050921     c                   OPEN      azorg01l
050700050831     c                   MOVEL     InpLnp        krgfil
050800010813     c     keyorg        CHAIN     azorg01l                           99
050900010813if  1c                   IF        NOT *in99
051000050831     C                   EVAL      OutDes = orgDes
051100050831     C                   EVAL      OutInd = orgInd
051200050831     C                   MOVEL     ORGCPF        OutCpf
051300050831     C                   EVAL      OutLoc = orgLoc
051400050831     C                   EVAL      OutPro = orgPro
051500050831     C                   EVAL      OutTel = orgTel
051600050831     C                   EVAL      OutFax = orgFax
051700050901     C                   EVAL      OutFil = orgFil
051701170316     C                   EVAL      outEmailFil = 'info' + %EDITC(inpLnp:'X')
051800010813e   1c                   ENDIF
051900050921     c                   CLOSE     azorg01l
052000170316     ** Per alcuni dati bisogna fare dei ragionamenti più raffinati.
052001170316     C                   RESET                   trulDscDs
052002170316     C                   EVAL      trulDscDs.iulDscFil = inpLnp
052003170316     C                   CALL      'TRULDSCR'
052004170316     C                   PARM                    trulDscDs
052005170316     C                   IF        trulDscDs.oulDscErr = *BLANK
052006170316     C                   EVAL      outDes = trulDscDs.oulDscDesc
052007170316     C                   EVAL      outTel = trulDscDs.oulDscTel
052008170316     C                   EVAL      outEmailFil = trulDscDs.oulDscMail
052009170316     C                   ENDIF
052010170316     C*
052100010813     C                   ENDSR
052200010813     C*--------------------------------------------------------------------------------------------*
052300010813      /TITLE Operazioni iniziali
052400010813     C*--------------------------------------------------------------------------------------------*
052500010813     C     *inzsr        BEGSR
052600010813     C*
052700010813     C* Ricevimento parametri da AS777 via "CONNECT SQL"
052800010813     C     *ENTRY        PLIST
052900050921     C     tis1650dsi    PARM                    rqsDta
053000050831     C                   PARM      tis1650dso    rpyDta
053100050902     C                   PARM                    esito
053200010813     c*
053300010813     c* chiavi di lettura
053400010813     c     keyorg        KLIST                                                  azorg01l
053500010813     c                   KFLD                    krgfil                         -filiale
053600051108     ** Attivo il controllo di sincronia.
053700051108     C                   EVAL      pCmd01 = 'STRCMTCTL LCKLVL(*CHG)'
053800051108     C                   CLEAR                   qcap0100
053900051108     C                   EVAL      qcabcsdh = '0'
054000051108     C                   EVAL      qcapa = '0'
054100051108     C                   EVAL      qcacmdss = '0'
054200051108     C                   EVAL      qcaerved = X'000000000000000000'
054300051108     C                   CLEAR                   qusec
054400051108     C                   EVAL      qusbprv = %SIZE(qusec)
054500051108     C                   CALLP(E)  pCmd(pCmd01:pCmd02:qcap0100:pCmd04:pCmd05:
054600051108     C                             pCmd06:pCmd07:pCmd08:qusec)
054700051108     C                   IF        (qusei <> *BLANK AND qusei <> 'CPF8351' AND
054800051108     C                             NOT %ERROR) OR %ERROR
054900051108     C                   CLEAR                   tis1650dso
055000051108     C                   EVAL      outErr = *ON
055100051108     C                   EVAL      esito = -1
055200051108     C                   EVAL      *INLR = *ON
055300051108     C                   RETURN
055400051108     C                   ENDIF
055500010813     c*
055600010813     c                   ENDSR
055700050921
055800050921     ***********************************************************************************************
055900050921     **?
056000051109     **?Controllo se è ancora possibile immettere disposizioni.
056100050921     **?
056200050921     ***********************************************************************************************
056300050921     C     chkFase       BEGSR
056400050921
056500051109     C                   EVAL      agc = atoi(%SUBST(inpDag:7:4))
056600051109     C                   EVAL      fgc = inpFgc
056700051109     C                   EVAL      ngc = inpNgc
056800051109     C                   CALLP(E)  tis7653r(inpKsu:agc:fgc:ngc:esito10)
056900051109     C                   IF        esito10 <> 0 OR %ERROR
057000050921     C                   EVAL      outErr = *ON
057100060606     C*                  EVAL      outMsg = 'poichè sono già state recepite -
057200060606     C*                            le disposizioni immesse in precedenza.'
057300060609     C                   EVAL      OutMsg = rtvMsgLang('TIS0675'
057400060609     C                             :inplang)
057500060609     C                   ENDIF
057600050921
057700050921     C                   ENDSR
057800060606
057900010813** MSG ***
058000010813Operazione eseguita con successo, disposizioni giacenza inoltrate.
058100010813Errori nell'operazione, disposizioni giacenza NON inoltrate.
