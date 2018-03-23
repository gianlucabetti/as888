000100030416     H*PARMS TGTRLS(*CURRENT)
000200030124     hDECEDIT('0,') DATEDIT(*DMY.)
000300030117      *****************************************************************
000400030122      *      stampa valor.giornaliera cooperative
000500030507      * ATTENZIONE: per inviarlo in sede non bisogna usare il comando INVIA
000600030507      * ma SAVRSTOBJ xchè è stato compilato con TGTRLS= *CURRENT
000700030117      *****************************************************************
000800030212     Ffictt03L  IF   E           K DISK                                         x Data Prestazione
000900030122     F                                     rename(Fictt000:Fictt003)
001000030120     Ffictd02L  IF   E           K DISK
001100030122     Ffifre02L  IF   E           K DISK
001200030122     Ffifpt01L  IF   E           K DISK
001300030121     Ftabel00f  IF   E           K DISK
001400030122     Ftntbe01l  IF   E           K DISK
001500030120     Fazorg01L  IF   E           K DISK
001600030131     Ffiapd01L  IF   E           K DISK
001700030121     FFICN22P   O    E             PRINTER oflind(*in01)
001800030117      *****************************************************************
001900030131      *  Riepilogo Indicatori utilizzati per la Stampa
002000030131      *  01 = (on) Overflow
002100030131      *  21 = Generico
002200030131      *  55 = (on) Stampa di riepilogo di periodo
002300030131      *  72 = (on) Stampa Informazioni Cod.filiale/Cooperativa
002400030131      *  75 = (on) Stampa il dettaglio riga x riga
002500030218      *  77 = (on) Stampa uso interno
002600030131      *  81 = (on) Stampa del Nastro Lavorativo Partenze
002700030131      *  82 = (on) Stampa del Nastro Lavorativo Arrivi
002800030131      *  83 = (on) Stampa del Nastro Lavorativo i Totali
002900030218      *  85 = (on) Stampa anche competenze a 0
003000030131      *  88 = (on) Stampa Allegato Fattura o Valorizz.Giornaliera
003100030131      *
003200030131      *****************************************************************
003300030129     D  CVOC           S              3s 0 dim(999)                             Voce in dettaglio
003400030129     D  RVOC           S              6s 0 dim(999)                             Raggr.+ Voce
003500030129     D  DVOC           S             25    dim(999)                             Descrizione Voce
003600030129     D  DUMA           S              7    dim(999)                             U.M. Voce
003700030129     D  DQTA           S             11S 2 dim(999)                             Qtà Voce
003800030129     D  DCOR           S             11S 3 dim(999)                             Corrispettivo Voce
003900030129     D  DCOM           S             11S 3 dim(999)                             Competenza    Voce
004000030131      *
004100030218     D  VRCD           S              7A   dim(999)                             Raggr+Cod Ret
004200030131     D  VCOD           S              3A   dim(999)                             Cod.Rettifica
004300030131     D  VDES           S             30A   dim(999)                             Des.Rettifica
004400030131     D  VVAL           S             11S 3 dim(999)                             Val.Rettifica
004500030129      *****************************************************************
004600030117     d KPJBA         E DS
004700030117     D CNCR80        E DS
004800030117     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004900030117     D fnlv24ds      E DS
005000030122     D Dcre          E DS
005100030121     D Ds8A          E DS
005200030122     D Ds8D          E DS
005300030122     D DsTR          E DS
005400030212     D ficn20ds      e DS
005500030218     d ds1z          e ds
005600030121      *
005700030121     D par36r1         DS
005800030121     D  dat36r1                       8s 0
005900030121     D  fgs36r1                       3s 0
006000030121     D  tip36r1                       1
006100030121     D  fgt36r1                            like(fgtds)
006200030121     D  err36r1                       1
006300030416      *-
006400030416     D fictt           DS
006500030416     D  maxddc                             like(cttddc)
006600030416     D  pdrctt                             like(cttpdr)
006700030523     D  mesectt                       6
006800030416     D  fgsctt                             like(cttfgs)
006900030416     D  itcctt                             like(cttitc)
007000030121      *-
007100030121     D fgtds         e ds                  extname(fifgt00f)
007200030218     D fifre         e ds                  extname(fifre00f)
007300030121      *-
007400030117     D WLBDAT          DS
007500030117     D  G02DAT                 1      8  0
007600030117     D  G02INV                 9     16  0
007700030117     D  G02ERR                17     17
007800030117     D  G02TGI                18     22  0
007900030121      *-
008000030120      * per ordinare letture se utente di sede richiesto ampio range
008100030120      * allora organizza la stampa per divisione/area
008200030120     D                 DS
008300030120     D  KRAGRF                 1      3
008400030120     D  KFILIA                 4      6
008500030120     D  KORDIN                 1      6
008600030120      *
008700030120     D KEY             S              6    DIM(1000)
008800030121      *
008900030122     D  Totcrp         S                   like(TotCor)                         Partenze
009000030122     D  Totcra         S                   like(TotCom)                         Arrivi
009100030129     D  Totcrt         S                   like(TotCom)                         totale
009200030122     D  Totcr          S                   like(TotCor)                         Corrispettivi
009300030122     D  Totcm          S                   like(TotCom)                         Competenze
009400030122     D  Totgcr         S             +1    like(TotCor)                         tot.gen.corrisp.
009500030122     D  Totgcm         S             +1    like(TotCom)                         tot.gen.competenze
009600030131     D  TotgcrP        S                   like(Totgcr)                         tot.gen.corrisp.
009700030131     D  TotgcrA        S                   like(Totgcr)                         tot.gen.corrisp.
009800030131     D  TotgcrT        S                   like(Totgcr)                         tot.gen.corrisp.
009900030122     D  savDDC         S                   like(CttDDC)                         salva Data
010000030122     D  savPDR         S                   like(CttPDR)                         salva cod Coop
010100030218     D  tsr            S                   like(CttTSR)                         salva Tipo Servizio
010200030416     D  pdrda          S                   like(Cttpdr)                         salva Tipo Servizio
010300030416     D  pdra           S                   like(Cttpdr)                         salva Tipo Servizio
010400030523     D  meseda         S              6                                         salva Tipo Servizio
010500030523     D  mesea          S              6                                         salva Tipo Servizio
010600030416     D  nffda          S                   like(Cttnff)                         salva Tipo Servizio
010700030416     D  nffa           S                   like(Cttnff)                         salva Tipo Servizio
010800030416     D  dftda          S                   like(Cttdft)                         salva Tipo Servizio
010900030416     D  dfta           S                   like(Cttdft)                         salva Tipo Servizio
011000030122     D  savTIP         S                   like(FreTIP)                         salva Tipo
011100030122     D  savCRE         S                   like(freCRE)                         Salva Cod.Rettifica
011200030716     D  savNFF         S                   like(f20NFF)                         Salva il n°Fattura
011300030721     D  savFOR         S                   like(f20FOR)                         Salva il n°Fornitore
011400030131      *
011500030131     D  savFGS         S                   like(CttFGS)                         Salva Fil.Gestione
011600030131     D  savCOP         S                   like(CttPDR)                         Salva Cod.Coop
011700030131     D stp_Dec_Coop    S              1
011800030131     D Rettifiche_SN   S              1
011900030131      *
012000030122     D  AlmenoUno      S              1  0
012100030127     D  Prima_Volta    S              1
012200030127     D  Prima_Testa    S              1
012300030212     D  primo          S              1
012400030212     D  dataiso        S               d   inz
012500030212     D  dataeur        S               d   inz datfmt(*eur)
012600030416      * decodifica mese
012700030416     D decm            S             10    DIM(12) ctdata
012800030117      *****************************************************************
012900030117     C     *ENTRY        PLIST
013000030117     C                   PARM                    KPJBA
013100030212     C                   movel     KPJBU         ficn20ds
013200030716      *
013300030723     c                   if        f20NFF  > 0      and
013400030723     c                             f20NFF <> savNFF  or
013500030723     c                             f20FOR <> *blank and
013600030721     c                             f20FOR <> savFOR
013700030716     c                   move      f20NFF        savNFF
013800030721     c                   move      f20FOR        savFOR
013900030716     c                   Z-add     0             page
014000030716     c                   end
014100030212     c* chiudi
014200030212     c                   if        f20ela = 'C'
014300030212     c                   goto      fine
014400030212     c                   end
014500030121      *
014600030121      * impostazioni iniziali
014700030120     c                   exsr      Decod_Ini
014800030120      *
014900030117      *--------------------------------------------------------------*
015000030117      *             M A I N      L I N E
015100030117      *--------------------------------------------------------------*
015200030122      *  azzera campi di totale generale
015300030122     c                   Z-add     0             totgcr
015400030122     c                   Z-add     0             totgcm
015500030131     c                   Z-add     0             totgcrP
015600030131     c                   Z-add     0             totgcrA
015700030131     c                   Z-add     0             totgcrT
015800030212      * Stampa le Valorizzazioni Giornaliere
015900030121     c                   Exsr      VAL_Giorn
016000030129     c   55              Exsr      PRT_Riepil
016100030122      *  Tot.Generale
016200030122     c                   if        AlmenoUno = 1
016300030122     c                   z-add     totgcr        GenCOR
016400030122     c                   z-add     totgcm        GenCOM
016500030129     C     *in01         ifeq      *on
016600030714     C*****              Write     TESPAG
016700030714     C                   Write     NUMPAG
016800030124     c                   z-add     11            cntrig
016900030716     C***** Ctl se stampare testata Fattura
017000030716     c                   Exsr      Int_fattura
017100030122     C                   Setoff                                       01
017200030124     c                   Else
017300030124     C                   Write     SAL2RI
017400030124     c                   add       1             cntrig
017500030124     c                   End
017600030122     c                   Write     TOTGEN
017700030508     c                   Write     SAL2RI
017800030508     c                   add       2             cntrig
017900030122     c                   end
018000030416     c* stampa contratti appalto e variazioni x servizi P,T,F
018100030415     c                   exsr      srtsr
018200030416     c*
018300030415     C     FINE          TAG
018400030415     c                   if        f20ela = 'C' or f20ela = 'L'
018500030415     C                   SETON                                        LR
018600030415     c                   else
018700030415     C                   SETON                                        rt
018800030415     c                   end
018900030415     c*****************************************************************
019000030415     c* stampa servizio P, T, F infondo a tutto
019100030415     c*****************************************************************
019200030415     c     srtsr         begsr
019300030507     c
019400030416     c* salto a nuova pagina solo se ho più filiali
019500030416     C                   IF        PRIMO = *ON and dafi <> afi
019600030714     C*****              WRITE     TESPAG
019700030714     C                   WRITE     NUMPAG
019800030416     C                   Z-ADD     11            CNTRIG
019900030716     C***** Ctl se stampare testata Fattura
020000030716     c                   Exsr      Int_fattura
020100030507     c                   setoff                                       01
020200030416     C                   END
020300030416     c* primo record ok stampa testata iniziale
020400030416     c                   exsr      srprimo
020500030416     c*
020600030218     C                   clear                   VRcd
020700030218     C                   clear                   VCod
020800030218     C                   clear                   VDes
020900030218     C                   clear                   VVal
021000030507     c* abilito la stampa della testata della cooperativa e del dettaglio
021100030507     c* delle variazioni
021200030417     c                   eval      Stp_Dec_Coop ='S'
021300030506     c                   eval      f20rie = 'N'
021400030507     c                   setoff                                       55
021500030416     c* se sono in allegato fattura devo testare i rif. fattura altrimenti
021600030416     c* prendo tutto
021700030416     c                   if        f20nff <> 0
021800030416     c                   eval      nffda = f20nff
021900030416     c                   eval      nffa = f20nff
022000030416     c                   eval      dftda = f20dft
022100030416     c                   eval      dfta = f20dft
022200030416     c                   else
022300030416     c                   eval      nffda = 0
022400030416     c                   eval      nffa = 999999999
022500030416     c                   eval      dftda = 0
022600030416     c                   eval      dfta = 99999999
022700030416     c                   end
022800030416     c*
022900030415     c                   do        2             bb                2 0
023000030218     c                   select
023100030218     c                   when      bb = 1
023200030218     c                   eval      tsr = 'P'
023300030218     c                   when      bb = 2
023400030218     c                   eval      tsr = 'T'
023500030218     c                   endsl
023600030415     c* decodifico il servizio
023700030415     c                   exsr      sr1z
023800030415     c* stampo l'importo del contratto di appalto
023900030415     C/EXEC SQL
024000030415     C+ DECLARE A1 CURSOR FOR SELECT max(cttddc), cttpdr,
024100030523     C+ substr(digits(cttddc), 1, 6), cttfgs, sum(cttitc) FROM fictt00f
024200030416     C+ WHERE cttpdr between :f20cda and :f20ca and cttddc between
024300030416     C+ :f20dti and :f20dtf and ctttsr = :tsr and cttdft between :dftda
024400030416     C+ and :dfta and cttnff between :nffda and :nffa GROUP BY cttfgs,
024500030523     C+ cttpdr, substr(digits(cttddc), 1, 6) ORDER BY cttfgs, cttpdr,
024600030523     C+ substr(digits(cttddc), 1, 6)
024700030415     C/END-EXEC
024800030415      *          apertura cursore
024900030415     C/EXEC SQL
025000030415     C+ OPEN A1
025100030415     C/END-EXEC
025200030415     C                   DOU       SqlCod = 100 OR SqlCod < 0
025300030415      *          lettura cursore
025400030415     C/EXEC SQL
025500030415     C+ Fetch Next From A1 Into :fictt
025600030415     C/END-EXEC
025700030415      *
025800030415     C                   SELECT
025900030415     C                   WHEN      SqlCod = 100
026000030415     C                   WHEN      SqlCod >= 0
026100030416     c* decodifica cooperative solo se non riepilogo
026200030416     c                   exsr      srdeccop
026300030416     c* stampa contratto di appalto
026400030416     c                   eval      dectsr = §1zdes
026500030416     c                   move      mesectt       mm                2 0
026600030416     c                   eval      pmese = decm(mm)
026700030416     c                   eval      impapp = itcctt
026800090217      *
026900090217      * controlla il salto pagina
027000090217     C     *in01         ifeq      *on
027100090217     C                   Write     NUMPAG
027200090217     c                   z-add     11            cntrig
027300090217     C                   Setoff                                       01
027400090217     c                   End
027500090217      *
027600030416     c                   write     tesapp
027700030424     C                   add       3             cntrig
027800030416     c* stampo variazioni
027900030416     c                   eval      pdrda = pdrctt
028000030416     c                   eval      pdra = pdrctt
028100030416     c                   eval      meseda = mesectt
028200030416     c                   eval      mesea = mesectt
028300030416     c                   exsr      srvar
028400030415     C                   OTHER
028500030415     C                   ENDSL
028600030415      *
028700030415     C                   ENDDO
028800030415     C/EXEC SQL
028900030415     C+ Close A1
029000030415     C/END-EXEC
029100030415     c                   EndDO
029200030416     c* stampo variazioni per affitto macchinone (servizio 'F') solo x
029300030416     c* uso interno
029400030416     c                   if        f20ore  = 'S'
029500030416     c                   write     SAL2RI
029600030416     c                   add       1             cntrig
029700030424     C                   SETON                                        73
029800030416     c* primo record ok stampa testata iniziale
029900030416     c                   exsr      srprimo
030000030416     c*
030100030416     c                   eval      pdrda = f20cda
030200030416     c                   eval      pdra = f20ca
030300030416     c                   movel     f20dti        com06             6
030400030416     c                   move      com06         meseda
030500030416     c                   movel     f20dtf        com06
030600030416     c                   move      com06         mesea
030700030416     c                   eval      tsr = 'F'
030800030416     c                   exsr      sr1z
030900030416     c                   exsr      srvar
031000030424     C                   SETOFF                                       73
031100030507     c                   end
031200030424     c                   endsr
031300030416     c*
031400030416     c*****************************************************************
031500030416     c* stampa variazioni
031600030416     c*****************************************************************
031700030416     c     srvar         begsr
031800030416     c                   move      'S'           prima_testa
031900030416     C                   clear                   savcre
032000030218     C/EXEC SQL
032100030416     C+ DECLARE B1 CURSOR FOR SELECT * FROM fifre00f WHERE fretip = 'C'
032200030523     C+ and frepdr between :pdrda and :pdra and substr(digits(freddc), 1,
032300030523     C+ 6) between :meseda and :mesea and fretsr = :tsr AND FREDDC
032400030523     C+ BETWEEN :F20DTI AND :F20DTF ORDER BY FREPDR, FRETSR, FREDDC,
032500030523     C+ FRECRE, FREPNO
032600030218     C/END-EXEC
032700030218      *          apertura cursore
032800030218     C/EXEC SQL
032900030416     C+ OPEN B1
033000030218     C/END-EXEC
033100030218     C                   DOU       SqlCod = 100 OR SqlCod < 0
033200030218      *          lettura cursore
033300030218     C/EXEC SQL
033400030416     C+ Fetch Next From B1 Into :fifre
033500030218     C/END-EXEC
033600030218      *
033700030218     C                   SELECT
033800030218     C                   WHEN      SqlCod = 100
033900030218     C                   WHEN      SqlCod >= 0
034000030416     c* solo per tipo servizio F controllo se stampare la decodifica coop
034100030416     c                   if        tsr = 'F'
034200030416     c                   movel     frepdr        fgsctt
034300030416     c                   movel     frepdr        pdrctt
034400030416     c                   exsr      srdeccop
034500030416     c                   end
034600030218     c* stampa variazioni
034700030218     c                   exsr      Variaz_Valor
034800030218     C                   OTHER
034900030218     C                   ENDSL
035000030218      *
035100030218     C                   ENDDO
035200030218     C/EXEC SQL
035300030416     C+ Close B1
035400030218     C/END-EXEC
035500030415     c                   endsr
035600030416     c*****************************************************************
035700030416     c* stampa intestazione cooperativa
035800030416     c*****************************************************************
035900030416     c     srprimo       begsr
036000030416     c                   if        primo = *off
036100030416     c                   exsr      srtesta
036200030416     c                   eval      primo = *on
036300030416     c                   end
036400030416     c                   endsr
036500030416     c*****************************************************************
036600030416     c* stampa intestazione cooperativa
036700030416     c*****************************************************************
036800030416     c     srdeccop      begsr
036900030506     c* decodifica cooperative
037000030506     c                   if        f20rie = 'N'
037100030416     c                   movel     fgsctt        cttfgs
037200030416     c                   movel     pdrctt        cttpdr
037300030416     c                   exsr      Decod_Coop
037400030416     c* stampa testatina filiale coop
037500030416     c                   if        *in72
037600030416     c                   write     TESFIL
037700030416     c                   add       2             cntrig
037800030416     c                   move      'S'           prima_testa
037900030416     c                   end
038000030506     c                   end
038100030416     c                   endsr
038200030121      *--------------------------------------------------------------*
038300030121      *  Valorizzazione Giornaliera
038400030121      *--------------------------------------------------------------*
038500030121     c     VAL_Giorn     BEGSR
038600030212     c                   eval      primo = *off
038700030121      *
038800030121      *  Imposta il ciclo ordinato per Primo livello con tutte le sotto filiali
038900030121     C     Dal_X         DO        Al_X          A
039000030121     c                   movel     key(A)        LvL1              3 0           Primo Lvl
039100030121     c                   move      key(A)        Fil_Da            3 0           Filiale
039200030121      *
039300030121     C                   exsr      Elab_Val
039400030121      *
039500030121     c                   EndDO
039600030121      *
039700030121     c                   ENDSR
039800030121      *--------------------------------------------------------------*
039900030212      *  stampo testata iniziale
040000030121      *--------------------------------------------------------------*
040100030212     c     srtesta       BEGSR
040200030212      *  Testata Inziale
040300030212     c                   if        f20nff = 0
040400030212     c                   move      f20dti        dataiso
040500030212     c                   move      dataiso       dataeur
040600030212     c                   move      dataeur       dadata
040700030212     c                   move      f20dtf        dataiso
040800030212     c                   move      dataiso       dataeur
040900030212     c                   move      dataeur       adata
041000030714     C*****              Write     TESPAG
041100030714     C                   Write     NUMPAG
041200030212     c                   Write     TesVAL
041300030212     c                   z-add     15            cntrig            3 0
041400030212     c                   else
041500030212     c                   eval      fatsoc = f20soc
041600030212     c                   eval      fatsod = f20rsc
041700030212     c                   eval      fatfrn = f20for
041800030212     c                   eval      fatfrd = f20rgs
041900030212     c                   eval      fatnft = f20nff
042000030212     c                   move      f20dft        dataiso
042100030212     c                   move      dataiso       dataeur
042200030212     c                   move      dataeur       fatdft
042300030714     c                   Write     NumPAG
042400030714     c                   Write     TesFAT
042500030212     c                   z-add     19            cntrig
042600030212     c                   end
042700030507     c                   setoff                                       01
042800030212     c                   ENDSR
042900030212      *--------------------------------------------------------------*
043000030212      *  Ciclo di elaborazione e stampa
043100030212      *--------------------------------------------------------------*
043200030212     c     Elab_Val      BEGSR
043300030127      *
043400030121      * Legge Testata Conteggi x FGS
043500030121     c     Fil_Da        setll     FiCtt03L
043600030121     c     Fil_Da        reade     FiCtt03L
043700030121      *
043800030121     c                   Dow       Not %Eof(FiCtt03L)
043900030127      *
044000030127      *  azzera campi di totale
044100030127     c                   Z-add     0             totcr
044200030127     c                   Z-add     0             totcm
044300030127     c                   Z-add     0             totcrp
044400030127     c                   Z-add     0             totcra
044500030121      *
044600030121      * se supera l'estremo superiore del range esce direttamente
044700030121      *  dal ciclo di lettura.
044800030212     c                   if        CttPDR > f20ca
044900030121     c                   leave
045000030121     c                   End
045100030121      *
045200030121      * Controlla se il record letto è OK
045300030121     c                   exsr      Check_Rec
045400030121      *---
045500030121     c                   IF        Ok_Record ='S'
045600030212     c* primo record ok stampa testata iniziale
045700030416     c                   exsr      srprimo
045800030131      *
045900030131     c                   exsr      Decod_Coop
046000030124      *
046100030122      * salva data, cod.cooperativa e tipo servizio
046200030121     c                   z-add     CttDDC        savDDC
046300030121     c                   z-add     CttPDR        savPDR
046400030213      *
046500030212     c* stampo nel totale giornaliero la dicitura di manca tariffa/
046600030212     c* confermato (solo se no allegato fattura)
046700030610     C                   CLEAR                   DesCon
046800030212     c                   if        f20nff = 0
046900030212     c                   select
047000030212     c                   when      cttfla = 'S'
047100030212     c                   eval      descon = 'MANCA TARIFFA'
047200030212     c                   when      cttfvl = 'C'
047300030212     c                   eval      descon = 'CONFERMATO'
047400030212     c                   endsl
047500030212     c                   end
047600030121      *
047700030129      *  Stampa Testata Conteggi solo se non in Riepilogo
047800030212     c     *in55         ifeq      *Off
047900030131     c                   exsr      PRT_Tes_Det
048000030212     c                   EndIf
048100030129      *
048200030121      *  Letture e STAMPA -> Elaborazione dettaglio
048300030121     c     KCtt          setll     FiCtd02L
048400030121     c     KCtt          Reade     FiCtd02L
048500030121     c                   Dow       not %Eof(FiCtd02L)
048600030121      * Decodifica il Dettaglio
048700030212     c                   clear                   Vocep
048800030212     c                   clear                   GioVoc
048900030124     c                   clear                   GioUma
049000030124     c                   clear                   GioQta
049100030124     c                   clear                   GioCor
049200030124     c                   clear                   GioCom
049300030121     c                   Exsr      Decod_Det
049400030121      *
049500030121      *   Stampa riga dettaglio
049600030121     C                   If        gioCOR > 0 or
049700030212     C                             gioCOM > 0 and f20ore  = 'S'
049800030129      *
049900030129     c                   Exsr      Add_a_Riepil
050000030129      *
050100030129      *  Solo se non in Riepilogo
050200030212     c     *in55         ifeq      *Off
050300030131     c     *in75         ifeq      *on
050400030131     c                   exsr      PRT_Det
050500030131     c                   EndIf
050600030212     c                   End
050700030122      *
050800030122      * se scritto almeno un dettaglio
050900030122     c     AlmenoUno     ifeq      0
051000030122     C                   z-add     1             AlmenoUno
051100030122     c                   End
051200030121     c                   EndIF
051300030121      *
051400030121     c     KCtt          Reade     FiCtd02L
051500030121     c                   EndDo
051600030121      *
051700030121      * Variazioni ai conteggi
051800030218     c                   move      'S'           prima_testa
051900030218     c                   eval      tsr = 'X'
052000030416     c                   exsr      sr1z
052100030218     C     KFre          setll     fifre02l
052200030218     C     KFre          reade     fifre02l
052300030218     c                   Dow       not %Eof(fifre02l)
052400030121     c                   exsr      Variaz_Valor
052500030218     C     KFre          reade     fifre02l
052600030218     c                   EndDO
052700030121      *
052800030122      * Totale del giorno
052900030122     c                   exsr      Totale_Del
053000030122      * Nastro Lavorativo
053100030122      *  solo se richiesto
053200030122     c   77              exsr      Tot_Nastro
053300030122      *
053400030121     c                   endIF
053500030121      *---
053600030121      * Rilegge Testata Conteggi x FGS
053700030121     c     Fil_Da        Reade     FiCtt03L
053800030121     c                   enddo
053900030121      *
054000030122      *
054100030121     c                   EndSR
054200030121      *--------------------------------------------------------------*
054300030121      *  Decodifica il record di dettaglio
054400030121      *--------------------------------------------------------------*
054500030121     c     Decod_Det     BEGSR
054600030121      *
054700030121      *  Descrizione
054800030121     C                   move      '8A'          TBLCOD
054900030121     C                   movel     *blank        TBLKEY
055000030121     C                   movel     ctdVOC        TBLKEY
055100030212     C                   move      ctdVOC        vocep
055200030204     c                   clear                   tipUMA            1
055300030121     c     KTab          chain     Tabel00f
055400030121     c                   clear                   gioVOC
055500030121     c                   if        %Found(Tabel00f)
055600030122     c                   movel     TBLuni        Ds8a
055700030122     c                   movel     §8aDes        gioVOC
055800030204     c                   movel     §8aTT0        tipUMA
055900030121     c                   endIf
056000030507     c* non stampa la voce se §8ausc = ' ' e la stampa è x la
056100030507     c* coop non
056200030507     c                   if        §8ausc = ' ' and f20ore = 'N'
056300030507     c                   goto      novoc
056400030507     c                   end
056500030121      *
056600030121      *  Reperimento Tariffa per U.M.
056700030121     c                   eval      dat36r1 = CtdDDC
056800030121     c                   eval      fgs36r1 = CtdFgs
056900030121     C                   eval      tip36r1 = CtdTsr
057000030121     C                   clear                   fgt36r1
057100030121     C                   clear                   err36r1
057200030121     C                   call      'FICN36R1'
057300030121     c                   parm                    par36R1
057400030124     c                   if        err36r1 <> '1'
057500030124     C                   eval      fgtds = fgt36r1
057600030124     c                   else
057700030124     c                   clear                   fgtds
057800030124     c                   end
057900030121      *
058000030121     c                   clear                   gioUMA
058100030122     C                   move      ctdVOC        CodVoc            3
058200030121     C     KFpt          Chain     FiFpt01l
058300030204      *  Se non c'è la tariffa prende dalla 8A il TPG per decoficare
058400030204      *   il dettaglio
058500030204     C                   if        not %Found(FiFpt01l)
058600030204     c                   movel     tipUMA        FptTPG
058700030204     c                   endIf
058800030204      *
058900030121     c                   select
059000030121     c                   when      FptTPG = '0'
059100030204     c                   movel     '100 Kg'      gioUMA
059200030122     c                   z-add     CtdPkF        gioQTA
059300030212     c                   div       100           gioqta
059400030121      *
059500030121     c                   when      FptTPG = '1'
059600030121     c                   movel     'Colli'       gioUMA
059700030121     c                   z-add     CtdNcl        gioQTA
059800030121      *
059900030121     c                   when      FptTPG = '2'
060000030121     c                   movel     'Sped'        gioUMA
060100030121     c                   z-add     CtdNus        gioQTA
060200030121      *
060300030121     c                   when      FptTPG = '3'
060400030121     c                   movel     'Kg.'         gioUMA
060500030121     c                   z-add     CtdPkF        gioQTA
060600031110      *
060700031110     c                   when      FptTPG = 'H'
060800031110     c                   movel     'Bancali'     gioUMA
060900031110     c                   z-add     CtdBAN        gioQTA
061000030121      *
061100030121     c                   endSl
061200030122      *
061300030122      * Decodifica tab.TR descrizione UMA.
061400030122      *   Da implementare in seguito <<<<<<<<<<<<<<<<<<
061500030122     C                   move      'TR'          TBLCOD
061600030122     C                   movel     *blank        TBLKEY
061700030122     C                   movel     FptTPG        TBLKEY
061800030122     c     KTab          chain     Tabel00f
061900030122     c                   if        %Found(Tabel00f)
062000030122     c                   movel     tblUNI        dsTR
062100030122     c                   endIf
062200030121      *
062300030121      * Corrispettivo   C.E./Dinamico
062400030121     c                   z-add     ctdItc        gioCOR
062500030121     c                   z-add     ctdIed        gioCOM
062600030123      *
062700030123      * Competenze C/E
062800030123     c                   clear                   Comp8A            3
062900030123     c                   select
063000030123     c                   when      §8ACE1 <> '000'
063100030123     c                   movel     §8ACE1        Comp8A
063200030123     c                   when      §8ACE2 <> '000'
063300030123     c                   movel     §8ACE2        Comp8A
063400030123     c                   when      §8ACE3 <> '000'
063500030123     c                   movel     §8ACE3        Comp8A
063600030123     c                   when      §8ACE4 <> '000'
063700030123     c                   movel     §8ACE4        Comp8A
063800030123     c                   endsl
063900030123      *
064000030123     C                   move      '8D'          TBLCOD
064100030123     C                   movel     *blank        TBLKEY
064200030123     C                   movel     Comp8A        TBLKEY
064300030123     c     KTab          chain     Tabel00f
064400030123     c                   clear                   ParArr
064500030123     c                   if        %Found(Tabel00f)
064600030123     c                   movel     TBLuni        Ds8D
064700030123     c                   movel     §8dPa         ParArr            1
064800030123     c                   endIf
064900030122      *
065000030122      * In Totale Partenza o Arrivi
065100030122     c                   if        ParArr = 'P'
065200030122     c                   add       ctdItc        totCRP
065300030122     c                   endIf
065400030122     c                   if        ParArr = 'A'
065500030122     c                   add       ctdItc        totCRA
065600030122     c                   endIf
065700030127      *
065800030127      *  Totalizza il dettaglio
065900030127     c                   add       ctdItc        totcr
066000030127     c                   add       ctdIed        totcm
066100030127      * Aggiunge a totale Generale
066200030127     c                   add       ctdItc        totgcr
066300030127     c                   add       ctdIed        totgcm
066400030121      *
066500030507     c     novoc         EndSR
066600030131      *--------------------------------------------------------------*
066700030131      *  Imposta nelle schiere di riepilogo
066800030131      *--------------------------------------------------------------*
066900030131     c     Add_a_Riepil  BEGSR
067000030131      *
067100030129      * cumula in schiere di riepilogo
067200030129     c                   z-add     1             xr                5 0
067300030131     c     ctdVOC        lookup    CVOC(xr)                               21
067400030129      *
067500030131     c                   if        *in(21) = *off
067600030129     c                   z-add     1             xr                5 0
067700030131     c     *zeros        lookup    CVOC(xr)                               21
067800030129      *  Voce in dettaglio
067900030129     c                   move      ctdVOC        CVOC(xr)
068000030129      *  Raggr.+ Voce
068100030131      *   per ora il Raggruppamento è sempre a (000) ma un domani
068200030131      *   servirà per poter gestire delle rotture di livello in stampa
068300030129     c                   movel     '000'         fld6              6
068400030129     c                   move      ctdVOC        fld6
068500030129     c                   move      fld6          RVOC(xr)
068600030129      *  Descrizione Voce
068700030129     c                   move      gioVOC        DVOC(xr)
068800030129      *  U.M. Voce
068900030129     c                   move      gioUMA        DUMA(xr)
069000030129     C                   end
069100030129      *  Somma sempre cumulando:
069200030129      *  Qtà Voce
069300030129     c                   add       gioQTA        DQTA(xr)
069400030129      *  Corrispettivo Voce
069500030129     c                   add       gioCOR        DCOR(xr)
069600030129      *  Competenza    Voce
069700030129     c                   add       gioCOM        DCOM(xr)
069800030129      *
069900030129     c                   EndSR
070000030131      *--------------------------------------------------------------*
070100030131      *  Imposta nelle schiere di riepilogo le Rettifiche
070200030131      *--------------------------------------------------------------*
070300030131     c     ADD_Var_RIEP  BEGSR
070400030131      *
070500030131      *  Se ci sono delle rettifiche imposta il flag che servirà
070600030131      *  per stmpare la testata di Riepilogo delle Variazioni
070700030131     c                   eval        Rettifiche_SN = 'S'
070800030131      *
070900030131      * cumula in schiere di riepilogo
071000030131     c                   z-add     1             xr                5 0
071100030131     c     varCOD        lookup    VCod(xr)                               21
071200030131      *
071300030131     c                   if        *in(21) = *off
071400030131     c                   z-add     1             xr                5 0
071500030131     c     *blank        lookup    VCod(xr)                               21
071600030131      *  Codice Variazione
071700030131     c                   move      VarCOD        VCod(xr)
071800030131      *  Raggr.+ Codice Variazione
071900030131      *   per ora il Raggruppamento è sempre a (000) ma un domani
072000030131      *   servirà per poter gestire delle rotture di livello in stampa
072100030131     c                   movel     '000'         fld6              6
072200030131     c                   move      VarCOD        fld6
072300030131     c                   move      fld6          VRcd(xr)
072400030218     c                   movel     tsr           VRcd(xr)
072500030131      *  Descrizione Variazione
072600030131     c                   move      VarDES        VDes(xr)
072700030131     C                   end
072800030131      *  Somma sempre cumulando:
072900030131      *  Valore Rettifica
073000030131     c                   add       varVAL        VVal(xr)
073100030131      *
073200030131     c                   EndSR
073300030129      *--------------------------------------------------------------*
073400030129      *  Variazioni alle Valorizzazioni
073500030129      *--------------------------------------------------------------*
073600030129     c     Variaz_Valor  BEGSR
073700030218      * Solo la prima volta stampa Testata delle Variazioni
073800030218     c                   if        Prima_testa = 'S' and not *in55
073900030218     c                   move      'N'           prima_testa
074000030424     c     cntrig        ifLT      56
074100030218     c                   write     SAL2RI
074200030218     c                   exsr      srTESVAR
074300030218     c                   add       2             cntrig
074400030218     c                   else
074500030218      * salta a pag.nuova
074600030218     c                   seton                                        01
074700030212     c                   end
074800030121     c                   endif
074900030127      * Solo a rottura di codice rettifica
075000030122     c                   if        freCRE <> savCRE
075100030122     c                   move      freCRE        varCOD
075200030122     c                   move      freCRE        savCRE
075300030122     C                   movel(P)  'CRE'         tbeCOD
075400030122     C                   movel(P)  freCRE        tbeKE1
075500030122     C                   movel(P)  *blank        tbeKE2
075600030122      * Codice Rettifica
075700030122     C     KTbe          chain     tntbe01l
075800030122     c                   if        %found(tntbe01l)
075900030122     C                   movel     tbeuni        dCRE
076000030122     C                   movel     §crDES        varDES
076100030127     C                   move      '8D'          TBLCOD
076200030127     C                   movel     *blank        TBLKEY
076300030127     C                   movel     §crVOC        TBLKEY
076400030127     c     KTab          chain     Tabel00f
076500030127     c                   clear                   ParArr
076600030127     c                   if        %Found(Tabel00f)
076700030127     c                   movel     TBLuni        Ds8D
076800030127     c                   movel     §8dPa         ParArr            1
076900030127     c                   endIf
077000030218     c                   end
077100030218     c                   endIf
077200030127      * In Totale Partenza o Arrivi
077300030218     c                   if        tsr = 'X'
077400030127     c                   if        ParArr = 'P'
077500030127     c                   add       freTIM        totCRP
077600030131     c                   add       freTIM        totgcrP
077700030131     c                   add       freTIM        totgcrT
077800030127     c                   endIf
077900030127     c                   if        ParArr = 'A'
078000030127     c                   add       freTIM        totCRA
078100030131     c                   add       freTIM        totgcrA
078200030131     c                   add       freTIM        totgcrT
078300030127     c                   endIf
078400030218     c*   Aggiunge l'importo ai totali
078500030131     c                   add       freTIM        totcr
078600030131     c                   add       freTIM        totgcr
078700030218     c                   endIf
078800030218     c                   z-add     freTIM        varVAL
078900030218     c* giro data variazione
079000030218     C                   Z-ADD     freDDC        G02INV
079100030218     C                   MOVEL     '3'           G02ERR
079200030218     C                   CALL      'XSRDA8'
079300030218     C                   PARM                    WLBDAT
079400030218     C     G02ERR        IFne      '1'
079500030218     C                   Z-ADD     G02dat        varDAT
079600030218     C                   ENDIF
079700030218      *  Solo se non in Riepilogo stampo il dettaglio
079800030218     c                   if        frepno = 1 and not *in55
079900030218     c                   exsr      PRT_Det_Var
080000030218     C                   ENDIF
080100030131      * Cumula su schiere da stampare in seguito x stampa Riepilogativa
080200030131     c                   exsr      ADD_Var_RIEP
080300030218      *  Solo se non in Riepilogo stampa le note
080400030218     c                   if        not *in55 and frenot <> *blanks
080500030218     c                   exsr      PRT_Det_Not
080600030218     c                   endIf
080700030121      *
080800030121     c                   EndSR
080900030218      *--------------------------------------------------------------*
081000030218      *  stampa intestazione rettifiche
081100030218      *--------------------------------------------------------------*
081200030218     c     srtesvar      BEGSR
081300030218     c*
081400030218     c                   clear                   dectvar
081500030424     C                   SELECT
081600030424     c                   when      tsr = 'X'
081700030218     c                   eval      dectvar = 'Variazioni al piano logistico ope-
081800030218     c                             rativo per'
081900030424     c                   when      tsr = 'F'
082000030424     c                   eval      dectvar = 'Costo figurativo per'
082100030424     c                   other
082200030218     c                   eval      dectvar = 'Variazioni a quanto previsto nel -
082300030218     c                             contratto di appalto di'
082400030424     c                   endsl
082500030218     c                   eval      dectvar = %trimr(dectvar) + ' ' + §1zdes
082600030218     c*
082700030218     c                   write     tesvar
082800030218     c*
082900030218     c                   EndSR
083000030415      *--------------------------------------------------------------*
083100030415     c* decodifico tipo servizio
083200030415      *--------------------------------------------------------------*
083300030415     c     sr1z          BEGSR
083400030415     c*
083500030415     c                   eval      tblcod = '1Z'
083600030415     c                   eval      tblkey = tsr
083700030415     c     ktab          chain     tabel00f
083800030415     c                   if        %found(tabel00f)
083900030415     c                   movel     tbluni        ds1z
084000030415     c                   else
084100030415     c                   clear                   ds1z
084200030415     c                   end
084300030415     c*
084400030415     c                   EndSR
084500030218      *--------------------------------------------------------------*
084600030218      *  Totale del giorno
084700030218      *--------------------------------------------------------------*
084800030218     c     Totale_Del    BEGSR
084900030121      *
085000030122     c                   z-add     gioDAT        totDAT
085100030127     c                   z-add     totcr         totCOR
085200030127     c                   z-add     totcm         totCOM
085300030129      *  Solo se non in Riepilogo
085400030212     c     *in55         ifeq      *Off
085500030122      * x OVRFLOW
085600030122     c     *in01         ifeq      *on
085700030122     c                   Setoff                                       01
085800030714     C*****              Write     TESPAG
085900030714     C                   Write     NUMPAG
086000030124     c                   z-add     11            cntrig
086100030716     C***** Ctl se stampare testata Fattura
086200030716     c                   Exsr      Int_fattura
086300030124     c                   Else
086400030127     c   75              write     SAL2RI
086500030127     c   75              add       1             cntrig
086600030124     c                   End
086700030127     c   75              write     TOTDEL
086800030127     c  N75              write     TOTDE1
086900030124     c                   add       1             cntrig
087000030212     c                   end
087100030122      *
087200030122     c                   EndSR
087300030122      *--------------------------------------------------------------*
087400030122      *  Totale Nastro Lavorativo e media oraria
087500030122      *--------------------------------------------------------------*
087600030122     c     Tot_Nastro    BEGSR
087700030122      *
087800030129      *  controlla se deve emettere le righe del nastro lavorativo
087900030129     c                   setoff                                       818283
088000030129     c                   if        cttHpa > 0
088100030129     c                   seton                                        81  83
088200030129     c                   end
088300030129     c                   if        cttHar > 0
088400030129     c                   seton                                        82  83
088500030129     c                   end
088600030129      *
088700030122     c                   z-add     cttHAR        totHAR
088800030122     c                   z-add     cttHPA        totHPA
088900030129     c     totHPA        add       totHAR        totHTT
089000030129     c     totcrA        add       totcrP        totcrT
089100030122      *
089200030122      *  Media Partenze/Arrivi
089300030124     c                   if        cttHar > 0
089400030122     c     totcrA        div       cttHar        totmdA
089500030124     c                   end
089600030124      *
089700030124     c                   if        cttHpa > 0
089800030122     c     totcrP        div       cttHpa        totmdP
089900030124     c                   end
090000030129      *
090100030129      *  Media su ore totali
090200030129     c                   if        totHtt > 0
090300030129     c     totcrT        div       totHtt        totmdT
090400030129     c                   end
090500030129      *  Solo se non in Riepilogo
090600030212     c     *in55         ifeq      *Off
090700030122      * x OVRFLOW
090800030122     c     *in01         ifeq      *on
090900030122     c                   Setoff                                       01
091000030714     C******             Write     TESPAG
091100030714     C                   Write     NUMPAG
091200030124     c                   z-add     11            cntrig
091300030716     C***** Ctl se stampare testata Fattura
091400030716     c                   Exsr      Int_fattura
091500030124     c                   Else
091600030129     c   83              write     SAL2RI
091700030129     c   83              add       1             cntrig
091800030124     c                   End
091900030129     c   83              write     TOTORE
092000030129     c   81              add       1             cntrig
092100030129     c   82              add       1             cntrig
092200030129     c   83              add       1             cntrig
092300030129      *
092400030212     c                   end
092500030122      *
092600030122     c                   EndSR
092700030122      *--------------------------------------------------------------*
092800030122      *  Controlla se il Record Letto è entro i parametri di selezione
092900030122      *--------------------------------------------------------------*
093000030122     c     Check_Rec     BEGSR
093100030122      *
093200030121     c                   move      'S'           Ok_Record         1
093300030121      *
093400030121      * se il cod.Coop letto è inferiore al parametro introduttivo
093500030212     C                   if        cttPDR < f20cda
093600030121     c                   move      'N'           Ok_Record
093700030121     c                   end
093800030121      *
093900030212      * se non appartiene alle cooperative ???
094000030212     C                   if        cttTSR <> 'X'
094100030212     c                   move      'N'           Ok_Record
094200030212     c                   end
094300030121      *
094400030121      * se all'interno del range di Date Selezionate
094500030212     C                   if        cttDDC < f20dti  or
094600030212     C                             cttDDC > f20dtf
094700030121     c                   move      'N'           Ok_Record
094800030121     c                   end
094900030212     c* controlli x allegato fattura
095000030212     c                   if        f20nff <> 0 and
095100030212     c* fornitore, società, data fattura, numero fattura
095200030212     C                             (cttsoc <> f20soc  or
095300030212     C                             cttcdf <> f20for  or
095400030212     C                             cttdft <> f20dft  or
095500030212     C                             cttnff <> f20nff)
095600030212     c                   move      'N'           Ok_Record
095700030212     c                   end
095800030121      *
095900030121      *  Gira la data per la testata
096000030121     c                   If        Ok_Record = 'S'
096100030121     C                   Z-ADD     CttDDC        G02INV
096200030121     C                   MOVEL     '3'           G02ERR
096300030121     C                   CALL      'XSRDA8'
096400030121     C                   PARM                    WLBDAT
096500030121     C     G02ERR        IFne      '1'
096600030121     C                   Z-ADD     G02dat        GioDAT
096700030121     C                   ENDIF
096800030121     c                   EndIF
096900030121      *
097000030121     c                   EndSR
097100030121      *--------------------------------------------------------------*
097200030131      *  Decodifiche Intestazione Coop
097300030121      *--------------------------------------------------------------*
097400030131     c     Decod_Coop    BEGSR
097500030121      *
097600030131     c                   setoff                                       72
097700030131      * Serve a rottura di Filiale / Coop
097800030131     c                   if        CttFGS <> SavFGS or
097900030131     c                             CttPDR <> SavCOP
098000030131      *
098100030131     c                   eval      SavFGS = CttFGS
098200030131     c                   eval      SavCOP = CttPDR
098300030131      *
098400030131      * decodifica la filiale
098500030131     c                   move      SavFGS        codFIL
098600030131     c     CttFGS        chain     azorg01l
098700030131     c                   clear                   desFIL
098800030131     c                   if        %found(azorg01l)
098900030131     c                   movel     orgdes        desFIL
099000030131     c                   EndIf
099100030131      *
099200030131      *  Decodifica la Filiale e il Codice Coop
099300030131     c                   move      SavCOP        codCOP
099400030131     C                   move      'C'           D1ctip            1
099500030131     C                   move      cttPDR        kpdr              7 0
099600030131     C     Kapd          chain     fiapd01l
099700030131     C                   clear                   desCOP
099800030131     c                   if        %Found(fiapd01l)
099900030306     C                   MOVEL     apdrsF        desCOP
100000030131     C                   End
100100030131      *
100200030131      * serve per stampare la testatina con i dati della Filiale Cooperativa
100300030722      *  non in Fatturazione
100400030722     c                   if        Stp_Dec_Coop ='S' and
100500030722     c                             f20nff = 0 and f20for <> *blank
100600030131     c                   seton                                        72
100700030131     C                   End
100800030131      *
100900030131     c                   End
101000030131      *
101100030131     c                   EndSR
101200030131      *--------------------------------------------------------------*
101300030131      *  Decodifiche Iniziali
101400030131      *--------------------------------------------------------------*
101500030131     c     Decod_Ini     BEGSR
101600030131      *
101700030205     C                   TIME                    W0140            14 0
101800030205     C                   MOVE      W0140         data_08           8
101900030205     C                   MOVEl     W0140         ora_06            6
102000030205     C                   MOVE      data_08       data10
102100030205     C                   MOVEL     ora_06        ora04
102200030204      * Parametri di stampa
102300030212     c                   move      f20cda        dacop
102400030212     c                   move      f20ca         acop
102500030212     c                   move      f20dti        dataiso
102600030212     c                   move      dataiso       dataeur
102700030212     c                   move      dataeur       dadata
102800030212     c                   move      f20dtf        dataiso
102900030212     c                   move      dataiso       dataeur
103000030212     c                   move      dataeur       adata
103100030212     c                   eval      orecmp = f20ore
103200030212     c******             eval      compe0 = f20com
103300030212     c                   eval      dettag = f20det
103400030212     c                   eval      riepil = f20rie
103500030212     c* no stampa allegato fattura
103600030212     c                   if        f20nff = 0
103700030714>    c                   write     NUMPAG
103800030714     c                   write     PRIPAG
103900030212     c                   end
104000030204      *
104100030122      * Valori iniziali
104200030122     C                   z-add     0             AlmenoUno
104300030122     C                   z-add     1             TBLKUT
104400030122     C                   move      'C'           savTIP
104500030127     c                   eval         prima_volta ='S'
104600030131     c                   eval        Rettifiche_SN = 'N'
104700030131      *
104800030212     c                   movel     f20cda        daFI              3
104900030212     c                   movel     f20ca         aFI               3
105000030203      * Forzatamente si vuole vedere l'intestazione Filiale/Cooperativa
105100030523      * no x allegato fattura a meno che il po di gestione sia di diverso
105200030523     c                   eval      stp_Dec_Coop ='S'
105300030523     c                   if        f20nff <> 0 and dafi = afi
105400030212     c                   eval      stp_Dec_Coop ='N'
105500030212     c                   end
105600030122      *
105700030121      * prende le filiali di gestione
105800030121     c                   move      *zeros        dakey             6
105900030121     c                   move      *all'9'       akey              6
106000030121      *
106100030212     c                   if        f20cda <> 0
106200030212     c                   movel     f20cda        dafgs             3
106300030121     c                   move      dafgs         dafil             3 0
106400030121     c     dafil         chain     azorg01l
106500030121     c                   if        %found(azorg01l)
106600030121     c                   movel     orgfc0        dakey             6
106700030121     c                   move      orgfil        dakey
106800030121     c                   EndIf
106900030121     c                   EndIf
107000030121      *
107100030212     c                   if        f20ca <> *all'9'
107200030212     c                   movel     f20ca         afgs              3
107300030121     c                   move      afgs          afil              3 0
107400030121     c     afil          chain     azorg01l
107500030121     c                   if        %found(azorg01l)
107600030121     c                   movel     orgfc0        akey              6
107700030121     c                   move      orgfil        akey
107800030121     c                   EndIf
107900030121     c                   Else
108000030121     c                   EndIf
108100030121      *
108200030121      * ----------------
108300030120     C*
108400030212     C* f20ore  => 'N' Non deve evidenziare le Competenze
108500030212     C* f20ore  => 'S' Mostra le Competenze ad uso interno   77=ON
108600030212     c                   if        f20ore  = 'N'
108700030120     c                   setoff                                       77
108800030120     c                   else
108900030120     c                   seton                                        77
109000030120     c                   end
109100030120     C*
109200030212     C* f20com  => 'N' Non deve evidenziare le Competenze a 0
109300030212     C* f20com  => 'S' Mostra le Competenze anche a 0        85=ON
109400030212     c                   if        f20com  = 'N'
109500030120     c                   setoff                                       85
109600030120     c                   else
109700030120     c                   seton                                        85
109800030120     c                   end
109900030120     C*
110000030212     C* f20det   => 'N' Deve stampare solo i Totali
110100030212     C* f20det   => 'S' Deve stampare tutto                   75=ON
110200030212     c                   if        f20det   = 'N'
110300030120     c                   setoff                                       75
110400030120     c                   else
110500030120     c                   seton                                        75
110600030120     c                   end
110700030129     C*
110800030212     C* f20rie   => 'N' Deve stampare per il periodo le voci in elenco
110900030212     C* f20rie   => 'S' Deve stampare per il periodo le voci in elenco
111000030212     c                   if        f20rie    = 'N'
111100030129     c                   setoff                                       55
111200030129     c                   else
111300030129     c                   seton                                        55
111400030129     c                   end
111500030212     c* allegato fattura
111600030212     c                   if        f20nff = 0
111700030212     c                   setoff                                       88
111800030212     c                   else
111900030212     c                   seton                                        88
112000030212     c                   end
112100030120     C*
112200030120     C                   Z-ADD     1             CODUT
112300030120     C                   CALL      'X§PARUT'
112400030120     C                   PARM                    UT§DSE
112500030120     C                   MOVEL     RAGUT         RSUT             20
112600030120     C                   MOVEL     REC80         CNCR80
112700030121      *
112800030121      *   Organizzazione schiera con aree e filiali
112900030121     c                   exsr      org_Div_area
113000030121      *
113100030121      *  determina i range di lettura in base alla schiera
113200030121      *   di ordinamento x Divisione_Area
113300030121      *    per ottimizzare le letture nel ciclo di elaborazione
113400030121     C                   exsr      Range_Ini
113500030129      *
113600030129      *  Pulizia schiere di Riepilogo
113700030129     C                   clear                   CVOC
113800030129     C                   clear                   RVOC
113900030129     C                   clear                   DVOC
114000030129     C                   clear                   DUMA
114100030129     C                   clear                   DQTA
114200030129     C                   clear                   DCOR
114300030129     C                   clear                   DCOM
114400030131     C                   clear                   VRcd
114500030131     C                   clear                   VCod
114600030131     C                   clear                   VDes
114700030131     C                   clear                   VVal
114800030120      *
114900030120     c                   EndSR
115000030121      *--------------------------------------------------------------*
115100030121      *  Prepara chiave di lettura per stampa da Utente di sede che
115200030121      *  ha chiesto un ampio range oppure di stampare tutto.
115300030121      *--------------------------------------------------------------*
115400030121     c     Org_Div_area  BEGSR
115500030121      *
115600030127     c                   z-add     0             ky                5 0
115700030131     C                   setoff                                       21
115800030121     C                   MOVEL     *HIVAL        KEY
115900030121      *
116000030121     C     *LOVAL        SETLL     azorg
116100030121     C                   do        999           A                 5 0
116200030131     C                   READ      azorg                                  21
116300030131     C  N21orgFAG        IFEQ      'F'
116400030121     C     orgFAG        OREQ      'A'
116500030121     C                   MOVE      orgFIL        KFILIA
116600030121     C     orgFC0        IFGT      0
116700030121     C                   MOVE      orgFC0        KRAGRF
116800030121     C                   ELSE
116900030121     C                   MOVE      orgFIL        KRAGRF
117000030121     C                   END
117100030121     C* CARICO RAGGR. + FIL X LETTURA TNCSB
117200030127     c                   add       1             Ky
117300030127     C                   MOVE      KORDIN        KEY(Ky)
117400030121     C*
117500030121     C                   END
117600030131     C  N21              END
117700030127     c                   z-add     Ky            Sav_A             5 0
117800030121     C*
117900030121     C* Riordina la schiera per chiave di lettura
118000030121     C                   SORTA     KEY
118100030121      *
118200030121     c                   EndSR
118300030121      *--------------------------------------------------------------*
118400030121      *  Imposta inizio ciclo di lettura
118500030121      *--------------------------------------------------------------*
118600030121     c     Range_Ini     BEGSR
118700030121      *
118800030121      * cerca da quale partire in base alle selezioni
118900030121      *  determina il range da - a di ordinamento della stampa
119000030121     c                   z-add     1             x                 5 0
119100030121     c                   if        dakey <> *all'0'
119200030121     c     dakey         lookup    key(x)                                 21
119300030121     c                   end
119400030121     c                   z-add     x             dal_X             5 0
119500030121      *
119600030121     c                   z-add     1             x                 5 0
119700030121     c                   if        akey <> *all'9'
119800030121     c     akey          lookup    key(x)                                 21
119900030121     c                   else
120000030121     c                   z-add     Sav_A         x
120100030121     c                   end
120200030121     c                   z-add     x             al_X              5 0
120300030121      *
120400030121     c                   EndSR
120500030121      *--------------------------------------------------------------*
120600030212      *  Stampa il dettaglio del RIEPILOGO di Periodo
120700030121      *--------------------------------------------------------------*
120800030129     c     PRT_Riepil    BEGSR
120900030121      *
121000030131      *  Riordina la schiera x Raggr./Voce
121100030131      *   per poi eseguire la stampa delle voci così riordinate
121200030131     c                   sorta     RVOC
121300030131      *
121400030131      *  Stampa testata per il dettaglio del Riepilogo
121500030131     c                   exsr      PRT_Tes_Det
121600030131      *
121700030131      * Esegue la Stampa di Riepilogo
121800030131     c                   do        999           xdo               5 0
121900030131      * solo le righe valide
122000030131      *  aggancia i dati da stampare
122100030131     c                   if        RVOC(xdo) <> 0
122200030131      *
122300030131      * Con La Voce aggancia tutti i dati che servno in stampa
122400030131      *  e stampa la riga di Riepilogo in Dettaglio
122500030131     c                   move      RVoc(xdo)     VocX              3 0
122600030131     c                   z-add     1             Xc                5 0
122700030131     c     VocX          lookup    CVoc(Xc)                               21
122800030131     c                   if        *in(21) = *on
122900030212     c                   movel     CVoc(Xc)      Vocep
123000030212     c                   movel     DVoc(Xc)      GioVoc
123100030131     c                   movel     DUma(Xc)      GioUma
123200030131     c                   z-add     DQta(Xc)      GioQta
123300030131     c                   z-add     DCor(Xc)      GioCor
123400030131     c                   z-add     DCom(Xc)      GioCom
123500030131     c                   exsr      PRT_Det
123600030131     c                   End
123700030131      *
123800030131     c                   end
123900030131      *
124000030131     c                   enddo
124100030129      *
124200030131      * Dopo il Dettaglio del RIEPILOGO deve stampare le Rettifiche
124300030131      *  cumulate su relative schiere
124400030131     c                   if        Rettifiche_SN = 'S'
124500030131     c                   exsr      PRT_Var_Riep
124600030131     c                   EndIf
124700030131      *
124800030129     c                   EndSR
124900030129      *--------------------------------------------------------------*
125000030131      *  Stampa in RIEPILOGO il dettaglio delle Rettifiche
125100030129      *--------------------------------------------------------------*
125200030131     c     PRT_Var_Riep  Begsr
125300030218     c*
125400030218     c                   clear                   tsrs              1
125500030131     c                   sorta     VRcd
125600030218     c* Esegue la Stampa di Riepilogo Rettifiche
125700030131     c                   do        999           xdo               5 0
125800030131     c                   if        VRcd(xdo) <> *blank
125900030218      *  Stampa testata per ogi tipo servizio
126000030218     c                   movel     vrcd(xdo)     tsr
126100030218     c                   if        tsr <> tsrs
126200030218     c                   eval      tsrs = tsr
126300030424     c                   exsr      sr1z
126400030424     c     cntrig        ifLT      56
126500030218     c                   write     SAL2RI
126600030218     c                   exsr      srTESVAR
126700030218     c                   add       2             cntrig
126800030218     c                   else
126900030218      * salta a pag.nuova
127000030218     c                   seton                                        01
127100030218     c                   end
127200030218     c                   end
127300030218      * Con La Voce aggancia tutti i dati che servono in stampa
127400030131      *  e stampa la riga di Riepilogo in Dettaglio
127500030131     c                   move      VRcd(xdo)     VCodX             3
127600030131     c                   z-add     1             Xc                5 0
127700030131     c     VCodX         lookup    VCod(Xc)                               21
127800030131     c                   if        *in(21) = *on
127900030131     c                   movel     VCod(Xc)      varCOD
128000030131     c                   movel     VDes(Xc)      varDES
128100030131     c                   z-add     VVal(Xc)      varVAL
128200030131     c                   exsr      PRT_Det_Var
128300030131     c                   End
128400030131     c                   end
128500030131     c                   enddo
128600030131      *
128700030131     c                   EndSR
128800030131      *--------------------------------------------------------------*
128900030131      *  Allegato alla fattura
129000030131      *--------------------------------------------------------------*
129100030131     c     ALL_Fattura   BEGSR
129200030131      *
129300030121      *
129400030121     c                   ENDSR
129500030122      *--------------------------------------------------------------*
129600030122      *  Definizioni
129700030122      *--------------------------------------------------------------*
129800030122     c     DEFCAM        BEGSR
129900030122      *
130000030122      * Klist
130100030122     C     Ktab          KLIST
130200030122     C                   KFLD                    TBLKUT
130300030122     C                   KFLD                    TBLCOD
130400030122     C                   KFLD                    TBLKEY
130500030122      *
130600030122     C     KTbe          KLIST
130700030122     C                   KFLD                    tbeCOD
130800030122     C                   KFLD                    tbeKE1
130900030122     C                   KFLD                    tbeKE2
131000030122      *
131100030122     C     KCtt          KLIST
131200030122     C                   KFLD                    CttFgs
131300030122     C                   KFLD                    CttPdr
131400030122     C                   KFLD                    CttTsr
131500030122     C                   KFLD                    CttDdc
131600030122      *
131700030122     C     KFpt          KLIST
131800030122     C                   KFLD                    FgtPdr
131900030122     C                   KFLD                    FgtSml
132000030122     C                   KFLD                    FgtTsr
132100030122     C                   KFLD                    FgtCtr
132200030122     C                   KFLD                    FgtPrg
132300030122     C                   KFLD                    CodVoc            3
132400030122      *
132500030122     C     KFre          KLIST
132600030122     C                   KFLD                    savtip
132700030122     C                   KFLD                    SavPdr
132800030122     C                   KFLD                    SavDdc
132900030218     C                   KFLD                    Tsr
133000030131     C*
133100030131     C     Kapd          KLIST
133200030131     C                   KFLD                    D1ctip
133300030131     C                   KFLD                    kpdr              7 0
133400030131      *
133500030122     c                   ENDSR
133600030131      *--------------------------------------------------------------*
133700030131      *  Per stampare Testata Dettaglio
133800030131      *--------------------------------------------------------------*
133900030131     c     PRT_Tes_Det   Begsr
134000030131      *
134100030131      * Per Stampare Testata del Dettaglio
134200030131     C     *in01         ifeq      *on
134300030424     C     cntrig        orgt      55
134400030714     C****               Write     TESPAG
134500030714     C                   Write     NUMPAG
134600030131     c                   z-add     11            cntrig
134700030716     C***** Ctl se stampare testata Fattura
134800030716     c                   Exsr      Int_fattura
134900030131     C                   Setoff                                       01
135000030131     c                   eval         prima_volta ='S'
135100030131     c                   Else
135200030131      * se no Overflow deve saltare 2 righe
135300030131     C                   Write     SAL2RI
135400030131     c                   add       1             cntrig
135500030131     c                   End
135600030131      *
135700030131      * x stampa dettaglio oppure riepilogativa
135800030131     C     *in75         ifeq      *on
135900030131     C     *in55         oreq      *on
136000030131      *
136100030131      *   Questo però non deve essere stampato in Riepilogo
136200030212     c     *in55         ifeq      *Off
136300030203     c   72              write     TESFIL
136400030203     c   72              add       2             cntrig
136500030131      *
136600030131     c                   Write     TESGIO
136700030131     c                   add       2             cntrig
136800030131     c                   EndIf
136900030131      *
137000030131     c                   Write     TESDET
137100030131     c                   add       1             cntrig
137200030131      *
137300030131     c                   else
137400030212      * x stampa non in dettaglio
137500030131      *
137600030131     c                   if           prima_volta ='S' or
137700030131     c                                *in77 = *on
137800030131     c                   Write     TESGIT
137900030131     c                   add       1             cntrig
138000030131     c                   eval         prima_volta ='N'
138100030131     c                   End
138200030131      *
138300030131     c                   End
138400030131      *
138500030131     c                   ENDSR
138600030131      *--------------------------------------------------------------*
138700030131      *  Per stampare Dettaglio
138800030131      *--------------------------------------------------------------*
138900030131     c     PRT_Det       Begsr
139000030131      *
139100030131      * x OVRFLOW
139200030131     c     *in01         ifeq      *on
139300030131     c                   Setoff                                       01
139400030714     C*****              Write     TESPAG
139500030714     C                   Write     NUMPAG
139600030716     c                   z-add     11            cntrig
139700030716     C***** Ctl se stampare testata Fattura
139800030716     c                   Exsr      Int_fattura
139900030131     c                   write     TESDET
140000030716     c                   add       1             cntrig
140100030131     c                   End
140200030131     c                   write     DETGIO
140300030131     c                   add       1             cntrig
140400030131      *
140500030131     c                   ENDSR
140600030131      *--------------------------------------------------------------*
140700030131      *  Per stampare Testata Variazioni
140800030131      *--------------------------------------------------------------*
140900030131     c     PRT_Tes_Var   Begsr
141000030131      * salta a pag.nuova
141100030218     c     *in01         ifeq      *on
141200030218     c                   Setoff                                       01
141300030714     c*****              write     TESPAG
141400030714     c                   write     NUMPAG
141500030716     c                   z-add     11            cntrig
141600030716     C***** Ctl se stampare testata Fattura
141700030716     c                   Exsr      Int_fattura
141800030218     c                   exsr      srTESVAR
141900030716     c                   add       1             cntrig
142000030131     c                   end
142100030131      *
142200030131     c                   ENDSR
142300030131      *--------------------------------------------------------------*
142400030131      *  Per stampare Dettaglio Variazioni
142500030131      *--------------------------------------------------------------*
142600030131     c     PRT_Det_Var   Begsr
142700030218     c* eventuale OWERFLOW
142800030218     c                   exsr      Prt_tes_var
142900030218     c*
143000030131     c                   write     DETVAR
143100030131     c                   add       1             cntrig
143200030218     c*
143300030218     c                   ENDSR
143400030218      *--------------------------------------------------------------*
143500030218      *  Stampa note di rettifiche
143600030218      *--------------------------------------------------------------*
143700030218     c     Prt_det_Not   BEGSR
143800030218     c* eventuale OWERFLOW
143900030218     c                   exsr      Prt_tes_var
144000030218     c*
144100030218     c                   movel     freNOT        varNOT
144200030218     c                   write     DETNOT
144300030218     c                   add       1             cntrig
144400030218     c*
144500030218     c                   EndSR
144600030131      *--------------------------------------------------------------*
144700030716     c*  Intestazione Fattura
144800030716      *--------------------------------------------------------------*
144900030716     c     Int_fattura   BegSR
145000030716     c*
145100030716     c*  solo se stiamo stampano la Fattura
145200030716     c                   if        f20NFF > 0
145300030716     c                   Write     TesFAT
145400030716     c                   z-add     19            cntrig
145500030716     c                   End
145600030716     c*
145700030716     c                   EndSR
145800030716      *--------------------------------------------------------------*
145900030416** decm
146000030416gennaio
146100030416febbraio
146200030416marzo
146300030416aprile
146400030416maggio
146500030416giugno
146600030416luglio
146700030416agosto
146800030416settembre
146900030416ottobre
147000030416novembre
147100030416dicembre
