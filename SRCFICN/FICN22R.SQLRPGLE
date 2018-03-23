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
013100170118      *
013200030212     C                   movel     KPJBU         ficn20ds
013300030716      *
013400030723     c                   if        f20NFF  > 0      and
013500030723     c                             f20NFF <> savNFF  or
013600030723     c                             f20FOR <> *blank and
013700030721     c                             f20FOR <> savFOR
013800030716     c                   move      f20NFF        savNFF
013900030721     c                   move      f20FOR        savFOR
014000030716     c                   Z-add     0             page
014100030716     c                   end
014200030212     c* chiudi
014300030212     c                   if        f20ela = 'C'
014400030212     c                   goto      fine
014500030212     c                   end
014600030121      *
014700030121      * impostazioni iniziali
014800030120     c                   exsr      Decod_Ini
014900030120      *
015000030117      *--------------------------------------------------------------*
015100030117      *             M A I N      L I N E
015200030117      *--------------------------------------------------------------*
015300030122      *  azzera campi di totale generale
015400030122     c                   Z-add     0             totgcr
015500030122     c                   Z-add     0             totgcm
015600030131     c                   Z-add     0             totgcrP
015700030131     c                   Z-add     0             totgcrA
015800030131     c                   Z-add     0             totgcrT
015900030212      * Stampa le Valorizzazioni Giornaliere
016000030121     c                   Exsr      VAL_Giorn
016100030129     c   55              Exsr      PRT_Riepil
016200030122      *  Tot.Generale
016300030122     c                   if        AlmenoUno = 1
016400030122     c                   z-add     totgcr        GenCOR
016500030122     c                   z-add     totgcm        GenCOM
016600030129     C     *in01         ifeq      *on
016700030714     C*****              Write     TESPAG
016800030714     C                   Write     NUMPAG
016900030124     c                   z-add     11            cntrig
017000030716     C***** Ctl se stampare testata Fattura
017100030716     c                   Exsr      Int_fattura
017200030122     C                   Setoff                                       01
017300030124     c                   Else
017400030124     C                   Write     SAL2RI
017500030124     c                   add       1             cntrig
017600030124     c                   End
017700030122     c                   Write     TOTGEN
017800030508     c                   Write     SAL2RI
017900030508     c                   add       2             cntrig
018000030122     c                   end
018100030416     c* stampa contratti appalto e variazioni x servizi P,T,F
018200030415     c                   exsr      srtsr
018300030416     c*
018400030415     C     FINE          TAG
018500030415     c                   if        f20ela = 'C' or f20ela = 'L'
018600030415     C                   SETON                                        LR
018700030415     c                   else
018800030415     C                   SETON                                        rt
018900030415     c                   end
019000030415     c*****************************************************************
019100030415     c* stampa servizio P, T, F infondo a tutto
019200030415     c*****************************************************************
019300030415     c     srtsr         begsr
019400030507     c
019500030416     c* salto a nuova pagina solo se ho più filiali
019600030416     C                   IF        PRIMO = *ON and dafi <> afi
019700030714     C*****              WRITE     TESPAG
019800030714     C                   WRITE     NUMPAG
019900030416     C                   Z-ADD     11            CNTRIG
020000030716     C***** Ctl se stampare testata Fattura
020100030716     c                   Exsr      Int_fattura
020200030507     c                   setoff                                       01
020300030416     C                   END
020400030416     c* primo record ok stampa testata iniziale
020500030416     c                   exsr      srprimo
020600030416     c*
020700030218     C                   clear                   VRcd
020800030218     C                   clear                   VCod
020900030218     C                   clear                   VDes
021000030218     C                   clear                   VVal
021100030507     c* abilito la stampa della testata della cooperativa e del dettaglio
021200030507     c* delle variazioni
021300030417     c                   eval      Stp_Dec_Coop ='S'
021400030506     c                   eval      f20rie = 'N'
021500030507     c                   setoff                                       55
021600030416     c* se sono in allegato fattura devo testare i rif. fattura altrimenti
021700030416     c* prendo tutto
021800030416     c                   if        f20nff <> 0
021900030416     c                   eval      nffda = f20nff
022000030416     c                   eval      nffa = f20nff
022100030416     c                   eval      dftda = f20dft
022200030416     c                   eval      dfta = f20dft
022300030416     c                   else
022400030416     c                   eval      nffda = 0
022500030416     c                   eval      nffa = 999999999
022600030416     c                   eval      dftda = 0
022700030416     c                   eval      dfta = 99999999
022800030416     c                   end
022900030416     c*
023000030415     c                   do        2             bb                2 0
023100030218     c                   select
023200030218     c                   when      bb = 1
023300030218     c                   eval      tsr = 'P'
023400030218     c                   when      bb = 2
023500030218     c                   eval      tsr = 'T'
023600030218     c                   endsl
023700030415     c* decodifico il servizio
023800030415     c                   exsr      sr1z
023900030415     c* stampo l'importo del contratto di appalto
024000030415     C/EXEC SQL
024100030415     C+ DECLARE A1 CURSOR FOR SELECT max(cttddc), cttpdr,
024200030523     C+ substr(digits(cttddc), 1, 6), cttfgs, sum(cttitc) FROM fictt00f
024300030416     C+ WHERE cttpdr between :f20cda and :f20ca and cttddc between
024400030416     C+ :f20dti and :f20dtf and ctttsr = :tsr and cttdft between :dftda
024500030416     C+ and :dfta and cttnff between :nffda and :nffa GROUP BY cttfgs,
024600030523     C+ cttpdr, substr(digits(cttddc), 1, 6) ORDER BY cttfgs, cttpdr,
024700030523     C+ substr(digits(cttddc), 1, 6)
024800030415     C/END-EXEC
024900030415      *          apertura cursore
025000030415     C/EXEC SQL
025100030415     C+ OPEN A1
025200030415     C/END-EXEC
025300030415     C                   DOU       SqlCod = 100 OR SqlCod < 0
025400030415      *          lettura cursore
025500030415     C/EXEC SQL
025600030415     C+ Fetch Next From A1 Into :fictt
025700030415     C/END-EXEC
025800030415      *
025900030415     C                   SELECT
026000030415     C                   WHEN      SqlCod = 100
026100030415     C                   WHEN      SqlCod >= 0
026200030416     c* decodifica cooperative solo se non riepilogo
026300030416     c                   exsr      srdeccop
026400030416     c* stampa contratto di appalto
026500030416     c                   eval      dectsr = §1zdes
026600030416     c                   move      mesectt       mm                2 0
026700030416     c                   eval      pmese = decm(mm)
026800030416     c                   eval      impapp = itcctt
026900090217      *
027000090217      * controlla il salto pagina
027100090217     C     *in01         ifeq      *on
027200090217     C                   Write     NUMPAG
027300090217     c                   z-add     11            cntrig
027400090217     C                   Setoff                                       01
027500090217     c                   End
027600090217      *
027700030416     c                   write     tesapp
027800030424     C                   add       3             cntrig
027900030416     c* stampo variazioni
028000030416     c                   eval      pdrda = pdrctt
028100030416     c                   eval      pdra = pdrctt
028200030416     c                   eval      meseda = mesectt
028300030416     c                   eval      mesea = mesectt
028400030416     c                   exsr      srvar
028500030415     C                   OTHER
028600030415     C                   ENDSL
028700030415      *
028800030415     C                   ENDDO
028900030415     C/EXEC SQL
029000030415     C+ Close A1
029100030415     C/END-EXEC
029200030415     c                   EndDO
029300030416     c* stampo variazioni per affitto macchinone (servizio 'F') solo x
029400030416     c* uso interno
029500151020     c                   exsr      repVpo
029600151020     c                   if        f20dtf < datavpo
029700030416     c                   if        f20ore  = 'S'
029800030416     c                   write     SAL2RI
029900030416     c                   add       1             cntrig
030000030424     C                   SETON                                        73
030100030416     c* primo record ok stampa testata iniziale
030200030416     c                   exsr      srprimo
030300030416     c*
030400030416     c                   eval      pdrda = f20cda
030500030416     c                   eval      pdra = f20ca
030600030416     c                   movel     f20dti        com06             6
030700030416     c                   move      com06         meseda
030800030416     c                   movel     f20dtf        com06
030900030416     c                   move      com06         mesea
031000030416     c                   eval      tsr = 'F'
031100030416     c                   exsr      sr1z
031200030416     c                   exsr      srvar
031300030424     C                   SETOFF                                       73
031400030507     c                   end
031500151020     c                   endif
031600030424     c                   endsr
031700030416     c*
031800151020     c*____________________________________________________________
031900151020     c     repVpo        begsr
032000151020     c*____________________________________________________________
032100151020     c                   clear                   dataVpo           8 0
032200151020     c                   movel     f20cda        fili              3
032300151020      *reperisco limiti da tabella per switch su nuove stime
032400151020     c                   movel(p)  'FICN34R'     tbeke1
032500151020     c                   movel(p)  fili          tbeke2
032600151020     c                   movel     'VPO'         tbecod
032700151020     c     ktbe          chain     tntbe01l
032800151020     c                   if        %found(tntbe01l)
032900151020     c                   movel     tbeuni        dataVpo
033000151020     C                   Z-ADD     dataVpo       G02inv
033100151020     C                   MOVEL     '3'           G02ERR
033200151020     C                   CALL      'XSRDA8'
033300151020     C                   PARM                    WLBDAT
033400151020     c                   if        g02err <> '0'
033500151020     c                   move      20391231      dataVpo
033600151020     c                   end
033700151020     c                   else
033800151020      *ricerco 999 abilitazione per tutti
033900151020     c                   if        dataVpo999 = 0
034000151020     c                   movel(p)  '999'         tbeke2
034100151020     c                   movel     'VPO'         tbecod
034200151020     c     ktbe          chain     tntbe01l
034300151020     c                   if        %found(tntbe01l)
034400151020     c                   movel     tbeuni        dataVpo
034500151020     c                   movel     tbeuni        dataVpo999        8 0
034600151020     C                   Z-ADD     dataVpo       G02inv
034700151020     C                   MOVEL     '3'           G02ERR
034800151020     C                   CALL      'XSRDA8'
034900151020     C                   PARM                    WLBDAT
035000151020     c                   if        g02err <> '0'
035100151020     c                   move      20391231      dataVpo
035200151020     c                   move      20391231      dataVpo999
035300151020     c                   end
035400151020     c                   endif
035500151020     c                   else
035600151020     c                   z-add     dataVpo999    dataVpo
035700151020     c                   endif
035800151020     c                   endif
035900151020     c                   endsr
036000030416     c*****************************************************************
036100030416     c* stampa variazioni
036200030416     c*****************************************************************
036300030416     c     srvar         begsr
036400030416     c                   move      'S'           prima_testa
036500030416     C                   clear                   savcre
036600030218     C/EXEC SQL
036700030416     C+ DECLARE B1 CURSOR FOR SELECT * FROM fifre00f WHERE fretip = 'C'
036800030523     C+ and frepdr between :pdrda and :pdra and substr(digits(freddc), 1,
036900030523     C+ 6) between :meseda and :mesea and fretsr = :tsr AND FREDDC
037000030523     C+ BETWEEN :F20DTI AND :F20DTF ORDER BY FREPDR, FRETSR, FREDDC,
037100030523     C+ FRECRE, FREPNO
037200030218     C/END-EXEC
037300030218      *          apertura cursore
037400030218     C/EXEC SQL
037500030416     C+ OPEN B1
037600030218     C/END-EXEC
037700030218     C                   DOU       SqlCod = 100 OR SqlCod < 0
037800030218      *          lettura cursore
037900030218     C/EXEC SQL
038000030416     C+ Fetch Next From B1 Into :fifre
038100030218     C/END-EXEC
038200030218      *
038300030218     C                   SELECT
038400030218     C                   WHEN      SqlCod = 100
038500030218     C                   WHEN      SqlCod >= 0
038600030416     c* solo per tipo servizio F controllo se stampare la decodifica coop
038700030416     c                   if        tsr = 'F'
038800030416     c                   movel     frepdr        fgsctt
038900030416     c                   movel     frepdr        pdrctt
039000030416     c                   exsr      srdeccop
039100030416     c                   end
039200030218     c* stampa variazioni
039300030218     c                   exsr      Variaz_Valor
039400030218     C                   OTHER
039500030218     C                   ENDSL
039600030218      *
039700030218     C                   ENDDO
039800030218     C/EXEC SQL
039900030416     C+ Close B1
040000030218     C/END-EXEC
040100030415     c                   endsr
040200030416     c*****************************************************************
040300030416     c* stampa intestazione cooperativa
040400030416     c*****************************************************************
040500030416     c     srprimo       begsr
040600030416     c                   if        primo = *off
040700030416     c                   exsr      srtesta
040800030416     c                   eval      primo = *on
040900030416     c                   end
041000030416     c                   endsr
041100030416     c*****************************************************************
041200030416     c* stampa intestazione cooperativa
041300030416     c*****************************************************************
041400030416     c     srdeccop      begsr
041500030506     c* decodifica cooperative
041600030506     c                   if        f20rie = 'N'
041700030416     c                   movel     fgsctt        cttfgs
041800030416     c                   movel     pdrctt        cttpdr
041900030416     c                   exsr      Decod_Coop
042000030416     c* stampa testatina filiale coop
042100030416     c                   if        *in72
042200030416     c                   write     TESFIL
042300030416     c                   add       2             cntrig
042400030416     c                   move      'S'           prima_testa
042500030416     c                   end
042600030506     c                   end
042700030416     c                   endsr
042800030121      *--------------------------------------------------------------*
042900030121      *  Valorizzazione Giornaliera
043000030121      *--------------------------------------------------------------*
043100030121     c     VAL_Giorn     BEGSR
043200030212     c                   eval      primo = *off
043300030121      *
043400030121      *  Imposta il ciclo ordinato per Primo livello con tutte le sotto filiali
043500030121     C     Dal_X         DO        Al_X          A
043600030121     c                   movel     key(A)        LvL1              3 0           Primo Lvl
043700030121     c                   move      key(A)        Fil_Da            3 0           Filiale
043800030121      *
043900030121     C                   exsr      Elab_Val
044000030121      *
044100030121     c                   EndDO
044200030121      *
044300030121     c                   ENDSR
044400030121      *--------------------------------------------------------------*
044500030212      *  stampo testata iniziale
044600030121      *--------------------------------------------------------------*
044700030212     c     srtesta       BEGSR
044800030212      *  Testata Inziale
044900030212     c                   if        f20nff = 0
045000030212     c                   move      f20dti        dataiso
045100030212     c                   move      dataiso       dataeur
045200030212     c                   move      dataeur       dadata
045300030212     c                   move      f20dtf        dataiso
045400030212     c                   move      dataiso       dataeur
045500030212     c                   move      dataeur       adata
045600030714     C*****              Write     TESPAG
045700030714     C                   Write     NUMPAG
045800030212     c                   Write     TesVAL
045900030212     c                   z-add     15            cntrig            3 0
046000030212     c                   else
046100030212     c                   eval      fatsoc = f20soc
046200030212     c                   eval      fatsod = f20rsc
046300030212     c                   eval      fatfrn = f20for
046400030212     c                   eval      fatfrd = f20rgs
046500030212     c                   eval      fatnft = f20nff
046600030212     c                   move      f20dft        dataiso
046700030212     c                   move      dataiso       dataeur
046800030212     c                   move      dataeur       fatdft
046900170118      **
047000170118     c                   exsr      RETRIEVE_PEC
047100170118      **
047200030714     c                   Write     NumPAG
047300030714     c                   Write     TesFAT
047400030212     c                   z-add     19            cntrig
047500030212     c                   end
047600030507     c                   setoff                                       01
047700030212     c                   ENDSR
047800170118      *----------------------------------------------------------------------
047900170118      *   reperisce la PEC se c'è
048000170118      *----------------------------------------------------------------------
048100170118     C     RETRIEVE_PEC  BEGSR
048200170118      *
048300170118     C                   clear                   indPEC
048400170118      *
048500170118     C                   if        f20cFis <> ' '
048600170118     C                   eval       prmI_CFisc = f20cFis
048700170118     C                   clear                   prmO_PECEML
048800170118     C                   CALL      'FICNPECR'
048900170118     C                   PARM                    prmI_CFisc       20
049000170118     C                   PARM                    prmO_PECEML     100
049100170118     C                   eval       indPEC = 'PEC: ' + %trim(prmO_PECEML)
049200170118     c                   end
049300170118      *
049400170118     C                   ENDSR
049500030212      *--------------------------------------------------------------*
049600030212      *  Ciclo di elaborazione e stampa
049700030212      *--------------------------------------------------------------*
049800030212     c     Elab_Val      BEGSR
049900030127      *
050000030121      * Legge Testata Conteggi x FGS
050100030121     c     Fil_Da        setll     FiCtt03L
050200030121     c     Fil_Da        reade     FiCtt03L
050300030121      *
050400030121     c                   Dow       Not %Eof(FiCtt03L)
050500030127      *
050600030127      *  azzera campi di totale
050700030127     c                   Z-add     0             totcr
050800030127     c                   Z-add     0             totcm
050900030127     c                   Z-add     0             totcrp
051000030127     c                   Z-add     0             totcra
051100030121      *
051200030121      * se supera l'estremo superiore del range esce direttamente
051300030121      *  dal ciclo di lettura.
051400030212     c                   if        CttPDR > f20ca
051500030121     c                   leave
051600030121     c                   End
051700030121      *
051800030121      * Controlla se il record letto è OK
051900030121     c                   exsr      Check_Rec
052000030121      *---
052100030121     c                   IF        Ok_Record ='S'
052200030212     c* primo record ok stampa testata iniziale
052300030416     c                   exsr      srprimo
052400030131      *
052500030131     c                   exsr      Decod_Coop
052600030124      *
052700030122      * salva data, cod.cooperativa e tipo servizio
052800030121     c                   z-add     CttDDC        savDDC
052900030121     c                   z-add     CttPDR        savPDR
053000030213      *
053100030212     c* stampo nel totale giornaliero la dicitura di manca tariffa/
053200030212     c* confermato (solo se no allegato fattura)
053300030610     C                   CLEAR                   DesCon
053400030212     c                   if        f20nff = 0
053500030212     c                   select
053600030212     c                   when      cttfla = 'S'
053700030212     c                   eval      descon = 'MANCA TARIFFA'
053800030212     c                   when      cttfvl = 'C'
053900030212     c                   eval      descon = 'CONFERMATO'
054000030212     c                   endsl
054100030212     c                   end
054200030121      *
054300030129      *  Stampa Testata Conteggi solo se non in Riepilogo
054400030212     c     *in55         ifeq      *Off
054500030131     c                   exsr      PRT_Tes_Det
054600030212     c                   EndIf
054700030129      *
054800030121      *  Letture e STAMPA -> Elaborazione dettaglio
054900030121     c     KCtt          setll     FiCtd02L
055000030121     c     KCtt          Reade     FiCtd02L
055100030121     c                   Dow       not %Eof(FiCtd02L)
055200030121      * Decodifica il Dettaglio
055300030212     c                   clear                   Vocep
055400030212     c                   clear                   GioVoc
055500030124     c                   clear                   GioUma
055600030124     c                   clear                   GioQta
055700030124     c                   clear                   GioCor
055800030124     c                   clear                   GioCom
055900030121     c                   Exsr      Decod_Det
056000030121      *
056100030121      *   Stampa riga dettaglio
056200030121     C                   If        gioCOR > 0 or
056300030212     C                             gioCOM > 0 and f20ore  = 'S'
056400030129      *
056500030129     c                   Exsr      Add_a_Riepil
056600030129      *
056700030129      *  Solo se non in Riepilogo
056800030212     c     *in55         ifeq      *Off
056900030131     c     *in75         ifeq      *on
057000030131     c                   exsr      PRT_Det
057100030131     c                   EndIf
057200030212     c                   End
057300030122      *
057400030122      * se scritto almeno un dettaglio
057500030122     c     AlmenoUno     ifeq      0
057600030122     C                   z-add     1             AlmenoUno
057700030122     c                   End
057800030121     c                   EndIF
057900030121      *
058000030121     c     KCtt          Reade     FiCtd02L
058100030121     c                   EndDo
058200030121      *
058300030121      * Variazioni ai conteggi
058400030218     c                   move      'S'           prima_testa
058500030218     c                   eval      tsr = 'X'
058600030416     c                   exsr      sr1z
058700030218     C     KFre          setll     fifre02l
058800030218     C     KFre          reade     fifre02l
058900030218     c                   Dow       not %Eof(fifre02l)
059000030121     c                   exsr      Variaz_Valor
059100030218     C     KFre          reade     fifre02l
059200030218     c                   EndDO
059300030121      *
059400030122      * Totale del giorno
059500030122     c                   exsr      Totale_Del
059600030122      * Nastro Lavorativo
059700030122      *  solo se richiesto
059800030122     c   77              exsr      Tot_Nastro
059900030122      *
060000030121     c                   endIF
060100030121      *---
060200030121      * Rilegge Testata Conteggi x FGS
060300030121     c     Fil_Da        Reade     FiCtt03L
060400030121     c                   enddo
060500030121      *
060600030122      *
060700030121     c                   EndSR
060800030121      *--------------------------------------------------------------*
060900030121      *  Decodifica il record di dettaglio
061000030121      *--------------------------------------------------------------*
061100030121     c     Decod_Det     BEGSR
061200030121      *
061300030121      *  Descrizione
061400030121     C                   move      '8A'          TBLCOD
061500030121     C                   movel     *blank        TBLKEY
061600030121     C                   movel     ctdVOC        TBLKEY
061700030212     C                   move      ctdVOC        vocep
061800030204     c                   clear                   tipUMA            1
061900030121     c     KTab          chain     Tabel00f
062000030121     c                   clear                   gioVOC
062100030121     c                   if        %Found(Tabel00f)
062200030122     c                   movel     TBLuni        Ds8a
062300030122     c                   movel     §8aDes        gioVOC
062400030204     c                   movel     §8aTT0        tipUMA
062500030121     c                   endIf
062600030507     c* non stampa la voce se §8ausc = ' ' e la stampa è x la
062700030507     c* coop non
062800030507     c                   if        §8ausc = ' ' and f20ore = 'N'
062900030507     c                   goto      novoc
063000030507     c                   end
063100030121      *
063200030121      *  Reperimento Tariffa per U.M.
063300030121     c                   eval      dat36r1 = CtdDDC
063400030121     c                   eval      fgs36r1 = CtdFgs
063500030121     C                   eval      tip36r1 = CtdTsr
063600030121     C                   clear                   fgt36r1
063700030121     C                   clear                   err36r1
063800030121     C                   call      'FICN36R1'
063900030121     c                   parm                    par36R1
064000030124     c                   if        err36r1 <> '1'
064100030124     C                   eval      fgtds = fgt36r1
064200030124     c                   else
064300030124     c                   clear                   fgtds
064400030124     c                   end
064500030121      *
064600030121     c                   clear                   gioUMA
064700030122     C                   move      ctdVOC        CodVoc            3
064800030121     C     KFpt          Chain     FiFpt01l
064900030204      *  Se non c'è la tariffa prende dalla 8A il TPG per decoficare
065000030204      *   il dettaglio
065100030204     C                   if        not %Found(FiFpt01l)
065200030204     c                   movel     tipUMA        FptTPG
065300030204     c                   endIf
065400030204      *
065500030121     c                   select
065600030121     c                   when      FptTPG = '0'
065700030204     c                   movel     '100 Kg'      gioUMA
065800030122     c                   z-add     CtdPkF        gioQTA
065900030212     c                   div       100           gioqta
066000030121      *
066100030121     c                   when      FptTPG = '1'
066200030121     c                   movel     'Colli'       gioUMA
066300030121     c                   z-add     CtdNcl        gioQTA
066400030121      *
066500030121     c                   when      FptTPG = '2'
066600030121     c                   movel     'Sped'        gioUMA
066700030121     c                   z-add     CtdNus        gioQTA
066800030121      *
066900030121     c                   when      FptTPG = '3'
067000030121     c                   movel     'Kg.'         gioUMA
067100030121     c                   z-add     CtdPkF        gioQTA
067200031110      *
067300031110     c                   when      FptTPG = 'H'
067400031110     c                   movel     'Bancali'     gioUMA
067500031110     c                   z-add     CtdBAN        gioQTA
067600030121      *
067700030121     c                   endSl
067800030122      *
067900030122      * Decodifica tab.TR descrizione UMA.
068000030122      *   Da implementare in seguito <<<<<<<<<<<<<<<<<<
068100030122     C                   move      'TR'          TBLCOD
068200030122     C                   movel     *blank        TBLKEY
068300030122     C                   movel     FptTPG        TBLKEY
068400030122     c     KTab          chain     Tabel00f
068500030122     c                   if        %Found(Tabel00f)
068600030122     c                   movel     tblUNI        dsTR
068700030122     c                   endIf
068800030121      *
068900030121      * Corrispettivo   C.E./Dinamico
069000030121     c                   z-add     ctdItc        gioCOR
069100030121     c                   z-add     ctdIed        gioCOM
069200030123      *
069300030123      * Competenze C/E
069400030123     c                   clear                   Comp8A            3
069500030123     c                   select
069600030123     c                   when      §8ACE1 <> '000'
069700030123     c                   movel     §8ACE1        Comp8A
069800030123     c                   when      §8ACE2 <> '000'
069900030123     c                   movel     §8ACE2        Comp8A
070000030123     c                   when      §8ACE3 <> '000'
070100030123     c                   movel     §8ACE3        Comp8A
070200030123     c                   when      §8ACE4 <> '000'
070300030123     c                   movel     §8ACE4        Comp8A
070400030123     c                   endsl
070500030123      *
070600030123     C                   move      '8D'          TBLCOD
070700030123     C                   movel     *blank        TBLKEY
070800030123     C                   movel     Comp8A        TBLKEY
070900030123     c     KTab          chain     Tabel00f
071000030123     c                   clear                   ParArr
071100030123     c                   if        %Found(Tabel00f)
071200030123     c                   movel     TBLuni        Ds8D
071300030123     c                   movel     §8dPa         ParArr            1
071400030123     c                   endIf
071500030122      *
071600030122      * In Totale Partenza o Arrivi
071700030122     c                   if        ParArr = 'P'
071800030122     c                   add       ctdItc        totCRP
071900030122     c                   endIf
072000030122     c                   if        ParArr = 'A'
072100030122     c                   add       ctdItc        totCRA
072200030122     c                   endIf
072300030127      *
072400030127      *  Totalizza il dettaglio
072500030127     c                   add       ctdItc        totcr
072600030127     c                   add       ctdIed        totcm
072700030127      * Aggiunge a totale Generale
072800030127     c                   add       ctdItc        totgcr
072900030127     c                   add       ctdIed        totgcm
073000030121      *
073100030507     c     novoc         EndSR
073200030131      *--------------------------------------------------------------*
073300030131      *  Imposta nelle schiere di riepilogo
073400030131      *--------------------------------------------------------------*
073500030131     c     Add_a_Riepil  BEGSR
073600030131      *
073700030129      * cumula in schiere di riepilogo
073800030129     c                   z-add     1             xr                5 0
073900030131     c     ctdVOC        lookup    CVOC(xr)                               21
074000030129      *
074100030131     c                   if        *in(21) = *off
074200030129     c                   z-add     1             xr                5 0
074300030131     c     *zeros        lookup    CVOC(xr)                               21
074400030129      *  Voce in dettaglio
074500030129     c                   move      ctdVOC        CVOC(xr)
074600030129      *  Raggr.+ Voce
074700030131      *   per ora il Raggruppamento è sempre a (000) ma un domani
074800030131      *   servirà per poter gestire delle rotture di livello in stampa
074900030129     c                   movel     '000'         fld6              6
075000030129     c                   move      ctdVOC        fld6
075100030129     c                   move      fld6          RVOC(xr)
075200030129      *  Descrizione Voce
075300030129     c                   move      gioVOC        DVOC(xr)
075400030129      *  U.M. Voce
075500030129     c                   move      gioUMA        DUMA(xr)
075600030129     C                   end
075700030129      *  Somma sempre cumulando:
075800030129      *  Qtà Voce
075900030129     c                   add       gioQTA        DQTA(xr)
076000030129      *  Corrispettivo Voce
076100030129     c                   add       gioCOR        DCOR(xr)
076200030129      *  Competenza    Voce
076300030129     c                   add       gioCOM        DCOM(xr)
076400030129      *
076500030129     c                   EndSR
076600030131      *--------------------------------------------------------------*
076700030131      *  Imposta nelle schiere di riepilogo le Rettifiche
076800030131      *--------------------------------------------------------------*
076900030131     c     ADD_Var_RIEP  BEGSR
077000030131      *
077100030131      *  Se ci sono delle rettifiche imposta il flag che servirà
077200030131      *  per stmpare la testata di Riepilogo delle Variazioni
077300030131     c                   eval        Rettifiche_SN = 'S'
077400030131      *
077500030131      * cumula in schiere di riepilogo
077600030131     c                   z-add     1             xr                5 0
077700030131     c     varCOD        lookup    VCod(xr)                               21
077800030131      *
077900030131     c                   if        *in(21) = *off
078000030131     c                   z-add     1             xr                5 0
078100030131     c     *blank        lookup    VCod(xr)                               21
078200030131      *  Codice Variazione
078300030131     c                   move      VarCOD        VCod(xr)
078400030131      *  Raggr.+ Codice Variazione
078500030131      *   per ora il Raggruppamento è sempre a (000) ma un domani
078600030131      *   servirà per poter gestire delle rotture di livello in stampa
078700030131     c                   movel     '000'         fld6              6
078800030131     c                   move      VarCOD        fld6
078900030131     c                   move      fld6          VRcd(xr)
079000030218     c                   movel     tsr           VRcd(xr)
079100030131      *  Descrizione Variazione
079200030131     c                   move      VarDES        VDes(xr)
079300030131     C                   end
079400030131      *  Somma sempre cumulando:
079500030131      *  Valore Rettifica
079600030131     c                   add       varVAL        VVal(xr)
079700030131      *
079800030131     c                   EndSR
079900030129      *--------------------------------------------------------------*
080000030129      *  Variazioni alle Valorizzazioni
080100030129      *--------------------------------------------------------------*
080200030129     c     Variaz_Valor  BEGSR
080300030218      * Solo la prima volta stampa Testata delle Variazioni
080400030218     c                   if        Prima_testa = 'S' and not *in55
080500030218     c                   move      'N'           prima_testa
080600030424     c     cntrig        ifLT      56
080700030218     c                   write     SAL2RI
080800030218     c                   exsr      srTESVAR
080900030218     c                   add       2             cntrig
081000030218     c                   else
081100030218      * salta a pag.nuova
081200030218     c                   seton                                        01
081300030212     c                   end
081400030121     c                   endif
081500030127      * Solo a rottura di codice rettifica
081600030122     c                   if        freCRE <> savCRE
081700030122     c                   move      freCRE        varCOD
081800030122     c                   move      freCRE        savCRE
081900030122     C                   movel(P)  'CRE'         tbeCOD
082000030122     C                   movel(P)  freCRE        tbeKE1
082100030122     C                   movel(P)  *blank        tbeKE2
082200030122      * Codice Rettifica
082300030122     C     KTbe          chain     tntbe01l
082400030122     c                   if        %found(tntbe01l)
082500030122     C                   movel     tbeuni        dCRE
082600030122     C                   movel     §crDES        varDES
082700030127     C                   move      '8D'          TBLCOD
082800030127     C                   movel     *blank        TBLKEY
082900030127     C                   movel     §crVOC        TBLKEY
083000030127     c     KTab          chain     Tabel00f
083100030127     c                   clear                   ParArr
083200030127     c                   if        %Found(Tabel00f)
083300030127     c                   movel     TBLuni        Ds8D
083400030127     c                   movel     §8dPa         ParArr            1
083500030127     c                   endIf
083600030218     c                   end
083700030218     c                   endIf
083800030127      * In Totale Partenza o Arrivi
083900030218     c                   if        tsr = 'X'
084000030127     c                   if        ParArr = 'P'
084100030127     c                   add       freTIM        totCRP
084200030131     c                   add       freTIM        totgcrP
084300030131     c                   add       freTIM        totgcrT
084400030127     c                   endIf
084500030127     c                   if        ParArr = 'A'
084600030127     c                   add       freTIM        totCRA
084700030131     c                   add       freTIM        totgcrA
084800030131     c                   add       freTIM        totgcrT
084900030127     c                   endIf
085000030218     c*   Aggiunge l'importo ai totali
085100030131     c                   add       freTIM        totcr
085200030131     c                   add       freTIM        totgcr
085300030218     c                   endIf
085400030218     c                   z-add     freTIM        varVAL
085500030218     c* giro data variazione
085600030218     C                   Z-ADD     freDDC        G02INV
085700030218     C                   MOVEL     '3'           G02ERR
085800030218     C                   CALL      'XSRDA8'
085900030218     C                   PARM                    WLBDAT
086000030218     C     G02ERR        IFne      '1'
086100030218     C                   Z-ADD     G02dat        varDAT
086200030218     C                   ENDIF
086300030218      *  Solo se non in Riepilogo stampo il dettaglio
086400030218     c                   if        frepno = 1 and not *in55
086500030218     c                   exsr      PRT_Det_Var
086600030218     C                   ENDIF
086700030131      * Cumula su schiere da stampare in seguito x stampa Riepilogativa
086800030131     c                   exsr      ADD_Var_RIEP
086900030218      *  Solo se non in Riepilogo stampa le note
087000030218     c                   if        not *in55 and frenot <> *blanks
087100030218     c                   exsr      PRT_Det_Not
087200030218     c                   endIf
087300030121      *
087400030121     c                   EndSR
087500030218      *--------------------------------------------------------------*
087600030218      *  stampa intestazione rettifiche
087700030218      *--------------------------------------------------------------*
087800030218     c     srtesvar      BEGSR
087900030218     c*
088000030218     c                   clear                   dectvar
088100030424     C                   SELECT
088200030424     c                   when      tsr = 'X'
088300030218     c                   eval      dectvar = 'Variazioni al piano logistico ope-
088400030218     c                             rativo per'
088500030424     c                   when      tsr = 'F'
088600030424     c                   eval      dectvar = 'Costo figurativo per'
088700030424     c                   other
088800030218     c                   eval      dectvar = 'Variazioni a quanto previsto nel -
088900030218     c                             contratto di appalto di'
089000030424     c                   endsl
089100030218     c                   eval      dectvar = %trimr(dectvar) + ' ' + §1zdes
089200030218     c*
089300030218     c                   write     tesvar
089400030218     c*
089500030218     c                   EndSR
089600030415      *--------------------------------------------------------------*
089700030415     c* decodifico tipo servizio
089800030415      *--------------------------------------------------------------*
089900030415     c     sr1z          BEGSR
090000030415     c*
090100030415     c                   eval      tblcod = '1Z'
090200030415     c                   eval      tblkey = tsr
090300030415     c     ktab          chain     tabel00f
090400030415     c                   if        %found(tabel00f)
090500030415     c                   movel     tbluni        ds1z
090600030415     c                   else
090700030415     c                   clear                   ds1z
090800030415     c                   end
090900030415     c*
091000030415     c                   EndSR
091100030218      *--------------------------------------------------------------*
091200030218      *  Totale del giorno
091300030218      *--------------------------------------------------------------*
091400030218     c     Totale_Del    BEGSR
091500030121      *
091600030122     c                   z-add     gioDAT        totDAT
091700030127     c                   z-add     totcr         totCOR
091800030127     c                   z-add     totcm         totCOM
091900030129      *  Solo se non in Riepilogo
092000030212     c     *in55         ifeq      *Off
092100030122      * x OVRFLOW
092200030122     c     *in01         ifeq      *on
092300030122     c                   Setoff                                       01
092400030714     C*****              Write     TESPAG
092500030714     C                   Write     NUMPAG
092600030124     c                   z-add     11            cntrig
092700030716     C***** Ctl se stampare testata Fattura
092800030716     c                   Exsr      Int_fattura
092900030124     c                   Else
093000030127     c   75              write     SAL2RI
093100030127     c   75              add       1             cntrig
093200030124     c                   End
093300030127     c   75              write     TOTDEL
093400030127     c  N75              write     TOTDE1
093500030124     c                   add       1             cntrig
093600030212     c                   end
093700030122      *
093800030122     c                   EndSR
093900030122      *--------------------------------------------------------------*
094000030122      *  Totale Nastro Lavorativo e media oraria
094100030122      *--------------------------------------------------------------*
094200030122     c     Tot_Nastro    BEGSR
094300030122      *
094400030129      *  controlla se deve emettere le righe del nastro lavorativo
094500030129     c                   setoff                                       818283
094600030129     c                   if        cttHpa > 0
094700030129     c                   seton                                        81  83
094800030129     c                   end
094900030129     c                   if        cttHar > 0
095000030129     c                   seton                                        82  83
095100030129     c                   end
095200030129      *
095300030122     c                   z-add     cttHAR        totHAR
095400030122     c                   z-add     cttHPA        totHPA
095500030129     c     totHPA        add       totHAR        totHTT
095600030129     c     totcrA        add       totcrP        totcrT
095700030122      *
095800030122      *  Media Partenze/Arrivi
095900030124     c                   if        cttHar > 0
096000030122     c     totcrA        div       cttHar        totmdA
096100030124     c                   end
096200030124      *
096300030124     c                   if        cttHpa > 0
096400030122     c     totcrP        div       cttHpa        totmdP
096500030124     c                   end
096600030129      *
096700030129      *  Media su ore totali
096800030129     c                   if        totHtt > 0
096900030129     c     totcrT        div       totHtt        totmdT
097000030129     c                   end
097100030129      *  Solo se non in Riepilogo
097200030212     c     *in55         ifeq      *Off
097300030122      * x OVRFLOW
097400030122     c     *in01         ifeq      *on
097500030122     c                   Setoff                                       01
097600030714     C******             Write     TESPAG
097700030714     C                   Write     NUMPAG
097800030124     c                   z-add     11            cntrig
097900030716     C***** Ctl se stampare testata Fattura
098000030716     c                   Exsr      Int_fattura
098100030124     c                   Else
098200030129     c   83              write     SAL2RI
098300030129     c   83              add       1             cntrig
098400030124     c                   End
098500030129     c   83              write     TOTORE
098600030129     c   81              add       1             cntrig
098700030129     c   82              add       1             cntrig
098800030129     c   83              add       1             cntrig
098900030129      *
099000030212     c                   end
099100030122      *
099200030122     c                   EndSR
099300030122      *--------------------------------------------------------------*
099400030122      *  Controlla se il Record Letto è entro i parametri di selezione
099500030122      *--------------------------------------------------------------*
099600030122     c     Check_Rec     BEGSR
099700030122      *
099800030121     c                   move      'S'           Ok_Record         1
099900030121      *
100000030121      * se il cod.Coop letto è inferiore al parametro introduttivo
100100030212     C                   if        cttPDR < f20cda
100200030121     c                   move      'N'           Ok_Record
100300030121     c                   end
100400030121      *
100500030212      * se non appartiene alle cooperative ???
100600030212     C                   if        cttTSR <> 'X'
100700030212     c                   move      'N'           Ok_Record
100800030212     c                   end
100900030121      *
101000030121      * se all'interno del range di Date Selezionate
101100030212     C                   if        cttDDC < f20dti  or
101200030212     C                             cttDDC > f20dtf
101300030121     c                   move      'N'           Ok_Record
101400030121     c                   end
101500030212     c* controlli x allegato fattura
101600030212     c                   if        f20nff <> 0 and
101700030212     c* fornitore, società, data fattura, numero fattura
101800030212     C                             (cttsoc <> f20soc  or
101900030212     C                             cttcdf <> f20for  or
102000030212     C                             cttdft <> f20dft  or
102100030212     C                             cttnff <> f20nff)
102200030212     c                   move      'N'           Ok_Record
102300030212     c                   end
102400030121      *
102500030121      *  Gira la data per la testata
102600030121     c                   If        Ok_Record = 'S'
102700030121     C                   Z-ADD     CttDDC        G02INV
102800030121     C                   MOVEL     '3'           G02ERR
102900030121     C                   CALL      'XSRDA8'
103000030121     C                   PARM                    WLBDAT
103100030121     C     G02ERR        IFne      '1'
103200030121     C                   Z-ADD     G02dat        GioDAT
103300030121     C                   ENDIF
103400030121     c                   EndIF
103500030121      *
103600030121     c                   EndSR
103700030121      *--------------------------------------------------------------*
103800030131      *  Decodifiche Intestazione Coop
103900030121      *--------------------------------------------------------------*
104000030131     c     Decod_Coop    BEGSR
104100030121      *
104200030131     c                   setoff                                       72
104300030131      * Serve a rottura di Filiale / Coop
104400030131     c                   if        CttFGS <> SavFGS or
104500030131     c                             CttPDR <> SavCOP
104600030131      *
104700030131     c                   eval      SavFGS = CttFGS
104800030131     c                   eval      SavCOP = CttPDR
104900030131      *
105000030131      * decodifica la filiale
105100030131     c                   move      SavFGS        codFIL
105200030131     c     CttFGS        chain     azorg01l
105300030131     c                   clear                   desFIL
105400030131     c                   if        %found(azorg01l)
105500030131     c                   movel     orgdes        desFIL
105600030131     c                   EndIf
105700030131      *
105800030131      *  Decodifica la Filiale e il Codice Coop
105900030131     c                   move      SavCOP        codCOP
106000030131     C                   move      'C'           D1ctip            1
106100030131     C                   move      cttPDR        kpdr              7 0
106200030131     C     Kapd          chain     fiapd01l
106300030131     C                   clear                   desCOP
106400030131     c                   if        %Found(fiapd01l)
106500030306     C                   MOVEL     apdrsF        desCOP
106600030131     C                   End
106700030131      *
106800030131      * serve per stampare la testatina con i dati della Filiale Cooperativa
106900030722      *  non in Fatturazione
107000030722     c                   if        Stp_Dec_Coop ='S' and
107100030722     c                             f20nff = 0 and f20for <> *blank
107200030131     c                   seton                                        72
107300030131     C                   End
107400030131      *
107500030131     c                   End
107600030131      *
107700030131     c                   EndSR
107800030131      *--------------------------------------------------------------*
107900030131      *  Decodifiche Iniziali
108000030131      *--------------------------------------------------------------*
108100030131     c     Decod_Ini     BEGSR
108200030131      *
108300030205     C                   TIME                    W0140            14 0
108400030205     C                   MOVE      W0140         data_08           8
108500030205     C                   MOVEl     W0140         ora_06            6
108600030205     C                   MOVE      data_08       data10
108700030205     C                   MOVEL     ora_06        ora04
108800030204      * Parametri di stampa
108900030212     c                   move      f20cda        dacop
109000030212     c                   move      f20ca         acop
109100030212     c                   move      f20dti        dataiso
109200030212     c                   move      dataiso       dataeur
109300030212     c                   move      dataeur       dadata
109400030212     c                   move      f20dtf        dataiso
109500030212     c                   move      dataiso       dataeur
109600030212     c                   move      dataeur       adata
109700030212     c                   eval      orecmp = f20ore
109800030212     c******             eval      compe0 = f20com
109900030212     c                   eval      dettag = f20det
110000030212     c                   eval      riepil = f20rie
110100030212     c* no stampa allegato fattura
110200030212     c                   if        f20nff = 0
110300030714>    c                   write     NUMPAG
110400030714     c                   write     PRIPAG
110500030212     c                   end
110600030204      *
110700030122      * Valori iniziali
110800030122     C                   z-add     0             AlmenoUno
110900030122     C                   z-add     1             TBLKUT
111000030122     C                   move      'C'           savTIP
111100030127     c                   eval         prima_volta ='S'
111200030131     c                   eval        Rettifiche_SN = 'N'
111300030131      *
111400030212     c                   movel     f20cda        daFI              3
111500030212     c                   movel     f20ca         aFI               3
111600030203      * Forzatamente si vuole vedere l'intestazione Filiale/Cooperativa
111700030523      * no x allegato fattura a meno che il po di gestione sia di diverso
111800030523     c                   eval      stp_Dec_Coop ='S'
111900030523     c                   if        f20nff <> 0 and dafi = afi
112000030212     c                   eval      stp_Dec_Coop ='N'
112100030212     c                   end
112200030122      *
112300030121      * prende le filiali di gestione
112400030121     c                   move      *zeros        dakey             6
112500030121     c                   move      *all'9'       akey              6
112600030121      *
112700030212     c                   if        f20cda <> 0
112800030212     c                   movel     f20cda        dafgs             3
112900030121     c                   move      dafgs         dafil             3 0
113000030121     c     dafil         chain     azorg01l
113100030121     c                   if        %found(azorg01l)
113200030121     c                   movel     orgfc0        dakey             6
113300030121     c                   move      orgfil        dakey
113400030121     c                   EndIf
113500030121     c                   EndIf
113600030121      *
113700030212     c                   if        f20ca <> *all'9'
113800030212     c                   movel     f20ca         afgs              3
113900030121     c                   move      afgs          afil              3 0
114000030121     c     afil          chain     azorg01l
114100030121     c                   if        %found(azorg01l)
114200030121     c                   movel     orgfc0        akey              6
114300030121     c                   move      orgfil        akey
114400030121     c                   EndIf
114500030121     c                   Else
114600030121     c                   EndIf
114700030121      *
114800030121      * ----------------
114900030120     C*
115000030212     C* f20ore  => 'N' Non deve evidenziare le Competenze
115100030212     C* f20ore  => 'S' Mostra le Competenze ad uso interno   77=ON
115200030212     c                   if        f20ore  = 'N'
115300030120     c                   setoff                                       77
115400030120     c                   else
115500030120     c                   seton                                        77
115600030120     c                   end
115700030120     C*
115800030212     C* f20com  => 'N' Non deve evidenziare le Competenze a 0
115900030212     C* f20com  => 'S' Mostra le Competenze anche a 0        85=ON
116000030212     c                   if        f20com  = 'N'
116100030120     c                   setoff                                       85
116200030120     c                   else
116300030120     c                   seton                                        85
116400030120     c                   end
116500030120     C*
116600030212     C* f20det   => 'N' Deve stampare solo i Totali
116700030212     C* f20det   => 'S' Deve stampare tutto                   75=ON
116800030212     c                   if        f20det   = 'N'
116900030120     c                   setoff                                       75
117000030120     c                   else
117100030120     c                   seton                                        75
117200030120     c                   end
117300030129     C*
117400030212     C* f20rie   => 'N' Deve stampare per il periodo le voci in elenco
117500030212     C* f20rie   => 'S' Deve stampare per il periodo le voci in elenco
117600030212     c                   if        f20rie    = 'N'
117700030129     c                   setoff                                       55
117800030129     c                   else
117900030129     c                   seton                                        55
118000030129     c                   end
118100030212     c* allegato fattura
118200030212     c                   if        f20nff = 0
118300030212     c                   setoff                                       88
118400030212     c                   else
118500030212     c                   seton                                        88
118600030212     c                   end
118700030120     C*
118800030120     C                   Z-ADD     1             CODUT
118900030120     C                   CALL      'X§PARUT'
119000030120     C                   PARM                    UT§DSE
119100030120     C                   MOVEL     RAGUT         RSUT             20
119200030120     C                   MOVEL     REC80         CNCR80
119300030121      *
119400030121      *   Organizzazione schiera con aree e filiali
119500030121     c                   exsr      org_Div_area
119600030121      *
119700030121      *  determina i range di lettura in base alla schiera
119800030121      *   di ordinamento x Divisione_Area
119900030121      *    per ottimizzare le letture nel ciclo di elaborazione
120000030121     C                   exsr      Range_Ini
120100030129      *
120200030129      *  Pulizia schiere di Riepilogo
120300030129     C                   clear                   CVOC
120400030129     C                   clear                   RVOC
120500030129     C                   clear                   DVOC
120600030129     C                   clear                   DUMA
120700030129     C                   clear                   DQTA
120800030129     C                   clear                   DCOR
120900030129     C                   clear                   DCOM
121000030131     C                   clear                   VRcd
121100030131     C                   clear                   VCod
121200030131     C                   clear                   VDes
121300030131     C                   clear                   VVal
121400030120      *
121500030120     c                   EndSR
121600030121      *--------------------------------------------------------------*
121700030121      *  Prepara chiave di lettura per stampa da Utente di sede che
121800030121      *  ha chiesto un ampio range oppure di stampare tutto.
121900030121      *--------------------------------------------------------------*
122000030121     c     Org_Div_area  BEGSR
122100030121      *
122200030127     c                   z-add     0             ky                5 0
122300030131     C                   setoff                                       21
122400030121     C                   MOVEL     *HIVAL        KEY
122500030121      *
122600030121     C     *LOVAL        SETLL     azorg
122700030121     C                   do        999           A                 5 0
122800030131     C                   READ      azorg                                  21
122900030131     C  N21orgFAG        IFEQ      'F'
123000030121     C     orgFAG        OREQ      'A'
123100030121     C                   MOVE      orgFIL        KFILIA
123200030121     C     orgFC0        IFGT      0
123300030121     C                   MOVE      orgFC0        KRAGRF
123400030121     C                   ELSE
123500030121     C                   MOVE      orgFIL        KRAGRF
123600030121     C                   END
123700030121     C* CARICO RAGGR. + FIL X LETTURA TNCSB
123800030127     c                   add       1             Ky
123900030127     C                   MOVE      KORDIN        KEY(Ky)
124000030121     C*
124100030121     C                   END
124200030131     C  N21              END
124300030127     c                   z-add     Ky            Sav_A             5 0
124400030121     C*
124500030121     C* Riordina la schiera per chiave di lettura
124600030121     C                   SORTA     KEY
124700030121      *
124800030121     c                   EndSR
124900030121      *--------------------------------------------------------------*
125000030121      *  Imposta inizio ciclo di lettura
125100030121      *--------------------------------------------------------------*
125200030121     c     Range_Ini     BEGSR
125300030121      *
125400030121      * cerca da quale partire in base alle selezioni
125500030121      *  determina il range da - a di ordinamento della stampa
125600030121     c                   z-add     1             x                 5 0
125700030121     c                   if        dakey <> *all'0'
125800030121     c     dakey         lookup    key(x)                                 21
125900030121     c                   end
126000030121     c                   z-add     x             dal_X             5 0
126100030121      *
126200030121     c                   z-add     1             x                 5 0
126300030121     c                   if        akey <> *all'9'
126400030121     c     akey          lookup    key(x)                                 21
126500030121     c                   else
126600030121     c                   z-add     Sav_A         x
126700030121     c                   end
126800030121     c                   z-add     x             al_X              5 0
126900030121      *
127000030121     c                   EndSR
127100030121      *--------------------------------------------------------------*
127200030212      *  Stampa il dettaglio del RIEPILOGO di Periodo
127300030121      *--------------------------------------------------------------*
127400030129     c     PRT_Riepil    BEGSR
127500030121      *
127600030131      *  Riordina la schiera x Raggr./Voce
127700030131      *   per poi eseguire la stampa delle voci così riordinate
127800030131     c                   sorta     RVOC
127900030131      *
128000030131      *  Stampa testata per il dettaglio del Riepilogo
128100030131     c                   exsr      PRT_Tes_Det
128200030131      *
128300030131      * Esegue la Stampa di Riepilogo
128400030131     c                   do        999           xdo               5 0
128500030131      * solo le righe valide
128600030131      *  aggancia i dati da stampare
128700030131     c                   if        RVOC(xdo) <> 0
128800030131      *
128900030131      * Con La Voce aggancia tutti i dati che servno in stampa
129000030131      *  e stampa la riga di Riepilogo in Dettaglio
129100030131     c                   move      RVoc(xdo)     VocX              3 0
129200030131     c                   z-add     1             Xc                5 0
129300030131     c     VocX          lookup    CVoc(Xc)                               21
129400030131     c                   if        *in(21) = *on
129500030212     c                   movel     CVoc(Xc)      Vocep
129600030212     c                   movel     DVoc(Xc)      GioVoc
129700030131     c                   movel     DUma(Xc)      GioUma
129800030131     c                   z-add     DQta(Xc)      GioQta
129900030131     c                   z-add     DCor(Xc)      GioCor
130000030131     c                   z-add     DCom(Xc)      GioCom
130100030131     c                   exsr      PRT_Det
130200030131     c                   End
130300030131      *
130400030131     c                   end
130500030131      *
130600030131     c                   enddo
130700030129      *
130800030131      * Dopo il Dettaglio del RIEPILOGO deve stampare le Rettifiche
130900030131      *  cumulate su relative schiere
131000030131     c                   if        Rettifiche_SN = 'S'
131100030131     c                   exsr      PRT_Var_Riep
131200030131     c                   EndIf
131300030131      *
131400030129     c                   EndSR
131500030129      *--------------------------------------------------------------*
131600030131      *  Stampa in RIEPILOGO il dettaglio delle Rettifiche
131700030129      *--------------------------------------------------------------*
131800030131     c     PRT_Var_Riep  Begsr
131900030218     c*
132000030218     c                   clear                   tsrs              1
132100030131     c                   sorta     VRcd
132200030218     c* Esegue la Stampa di Riepilogo Rettifiche
132300030131     c                   do        999           xdo               5 0
132400030131     c                   if        VRcd(xdo) <> *blank
132500030218      *  Stampa testata per ogi tipo servizio
132600030218     c                   movel     vrcd(xdo)     tsr
132700030218     c                   if        tsr <> tsrs
132800030218     c                   eval      tsrs = tsr
132900030424     c                   exsr      sr1z
133000030424     c     cntrig        ifLT      56
133100030218     c                   write     SAL2RI
133200030218     c                   exsr      srTESVAR
133300030218     c                   add       2             cntrig
133400030218     c                   else
133500030218      * salta a pag.nuova
133600030218     c                   seton                                        01
133700030218     c                   end
133800030218     c                   end
133900030218      * Con La Voce aggancia tutti i dati che servono in stampa
134000030131      *  e stampa la riga di Riepilogo in Dettaglio
134100030131     c                   move      VRcd(xdo)     VCodX             3
134200030131     c                   z-add     1             Xc                5 0
134300030131     c     VCodX         lookup    VCod(Xc)                               21
134400030131     c                   if        *in(21) = *on
134500030131     c                   movel     VCod(Xc)      varCOD
134600030131     c                   movel     VDes(Xc)      varDES
134700030131     c                   z-add     VVal(Xc)      varVAL
134800030131     c                   exsr      PRT_Det_Var
134900030131     c                   End
135000030131     c                   end
135100030131     c                   enddo
135200030131      *
135300030131     c                   EndSR
135400030131      *--------------------------------------------------------------*
135500030131      *  Allegato alla fattura
135600030131      *--------------------------------------------------------------*
135700030131     c     ALL_Fattura   BEGSR
135800030131      *
135900030121      *
136000030121     c                   ENDSR
136100030122      *--------------------------------------------------------------*
136200030122      *  Definizioni
136300030122      *--------------------------------------------------------------*
136400030122     c     DEFCAM        BEGSR
136500030122      *
136600030122      * Klist
136700030122     C     Ktab          KLIST
136800030122     C                   KFLD                    TBLKUT
136900030122     C                   KFLD                    TBLCOD
137000030122     C                   KFLD                    TBLKEY
137100030122      *
137200030122     C     KTbe          KLIST
137300030122     C                   KFLD                    tbeCOD
137400030122     C                   KFLD                    tbeKE1
137500030122     C                   KFLD                    tbeKE2
137600030122      *
137700030122     C     KCtt          KLIST
137800030122     C                   KFLD                    CttFgs
137900030122     C                   KFLD                    CttPdr
138000030122     C                   KFLD                    CttTsr
138100030122     C                   KFLD                    CttDdc
138200030122      *
138300030122     C     KFpt          KLIST
138400030122     C                   KFLD                    FgtPdr
138500030122     C                   KFLD                    FgtSml
138600030122     C                   KFLD                    FgtTsr
138700030122     C                   KFLD                    FgtCtr
138800030122     C                   KFLD                    FgtPrg
138900030122     C                   KFLD                    CodVoc            3
139000030122      *
139100030122     C     KFre          KLIST
139200030122     C                   KFLD                    savtip
139300030122     C                   KFLD                    SavPdr
139400030122     C                   KFLD                    SavDdc
139500030218     C                   KFLD                    Tsr
139600030131     C*
139700030131     C     Kapd          KLIST
139800030131     C                   KFLD                    D1ctip
139900030131     C                   KFLD                    kpdr              7 0
140000030131      *
140100030122     c                   ENDSR
140200030131      *--------------------------------------------------------------*
140300030131      *  Per stampare Testata Dettaglio
140400030131      *--------------------------------------------------------------*
140500030131     c     PRT_Tes_Det   Begsr
140600030131      *
140700030131      * Per Stampare Testata del Dettaglio
140800030131     C     *in01         ifeq      *on
140900030424     C     cntrig        orgt      55
141000030714     C****               Write     TESPAG
141100030714     C                   Write     NUMPAG
141200030131     c                   z-add     11            cntrig
141300030716     C***** Ctl se stampare testata Fattura
141400030716     c                   Exsr      Int_fattura
141500030131     C                   Setoff                                       01
141600030131     c                   eval         prima_volta ='S'
141700030131     c                   Else
141800030131      * se no Overflow deve saltare 2 righe
141900030131     C                   Write     SAL2RI
142000030131     c                   add       1             cntrig
142100030131     c                   End
142200030131      *
142300030131      * x stampa dettaglio oppure riepilogativa
142400030131     C     *in75         ifeq      *on
142500030131     C     *in55         oreq      *on
142600030131      *
142700030131      *   Questo però non deve essere stampato in Riepilogo
142800030212     c     *in55         ifeq      *Off
142900030203     c   72              write     TESFIL
143000030203     c   72              add       2             cntrig
143100030131      *
143200030131     c                   Write     TESGIO
143300030131     c                   add       2             cntrig
143400030131     c                   EndIf
143500030131      *
143600030131     c                   Write     TESDET
143700030131     c                   add       1             cntrig
143800030131      *
143900030131     c                   else
144000030212      * x stampa non in dettaglio
144100030131      *
144200030131     c                   if           prima_volta ='S' or
144300030131     c                                *in77 = *on
144400030131     c                   Write     TESGIT
144500030131     c                   add       1             cntrig
144600030131     c                   eval         prima_volta ='N'
144700030131     c                   End
144800030131      *
144900030131     c                   End
145000030131      *
145100030131     c                   ENDSR
145200030131      *--------------------------------------------------------------*
145300030131      *  Per stampare Dettaglio
145400030131      *--------------------------------------------------------------*
145500030131     c     PRT_Det       Begsr
145600030131      *
145700030131      * x OVRFLOW
145800030131     c     *in01         ifeq      *on
145900030131     c                   Setoff                                       01
146000030714     C*****              Write     TESPAG
146100030714     C                   Write     NUMPAG
146200030716     c                   z-add     11            cntrig
146300030716     C***** Ctl se stampare testata Fattura
146400030716     c                   Exsr      Int_fattura
146500030131     c                   write     TESDET
146600030716     c                   add       1             cntrig
146700030131     c                   End
146800030131     c                   write     DETGIO
146900030131     c                   add       1             cntrig
147000030131      *
147100030131     c                   ENDSR
147200030131      *--------------------------------------------------------------*
147300030131      *  Per stampare Testata Variazioni
147400030131      *--------------------------------------------------------------*
147500030131     c     PRT_Tes_Var   Begsr
147600030131      * salta a pag.nuova
147700030218     c     *in01         ifeq      *on
147800030218     c                   Setoff                                       01
147900030714     c*****              write     TESPAG
148000030714     c                   write     NUMPAG
148100030716     c                   z-add     11            cntrig
148200030716     C***** Ctl se stampare testata Fattura
148300030716     c                   Exsr      Int_fattura
148400030218     c                   exsr      srTESVAR
148500030716     c                   add       1             cntrig
148600030131     c                   end
148700030131      *
148800030131     c                   ENDSR
148900030131      *--------------------------------------------------------------*
149000030131      *  Per stampare Dettaglio Variazioni
149100030131      *--------------------------------------------------------------*
149200030131     c     PRT_Det_Var   Begsr
149300030218     c* eventuale OWERFLOW
149400030218     c                   exsr      Prt_tes_var
149500030218     c*
149600030131     c                   write     DETVAR
149700030131     c                   add       1             cntrig
149800030218     c*
149900030218     c                   ENDSR
150000030218      *--------------------------------------------------------------*
150100030218      *  Stampa note di rettifiche
150200030218      *--------------------------------------------------------------*
150300030218     c     Prt_det_Not   BEGSR
150400030218     c* eventuale OWERFLOW
150500030218     c                   exsr      Prt_tes_var
150600030218     c*
150700030218     c                   movel     freNOT        varNOT
150800030218     c                   write     DETNOT
150900030218     c                   add       1             cntrig
151000030218     c*
151100030218     c                   EndSR
151200030131      *--------------------------------------------------------------*
151300030716     c*  Intestazione Fattura
151400030716      *--------------------------------------------------------------*
151500030716     c     Int_fattura   BegSR
151600030716     c*
151700030716     c*  solo se stiamo stampano la Fattura
151800030716     c                   if        f20NFF > 0
151900030716     c                   Write     TesFAT
152000030716     c                   z-add     19            cntrig
152100030716     c                   End
152200030716     c*
152300030716     c                   EndSR
152400170105      *----------------------------------------------------------------------
152500030416** decm
152600030416gennaio
152700030416febbraio
152800030416marzo
152900030416aprile
153000030416maggio
153100030416giugno
153200030416luglio
153300030416agosto
153400030416settembre
153500030416ottobre
153600030416novembre
153700030416dicembre
