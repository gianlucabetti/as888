000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200051014     H**********************************************************************
000300051014     c* 900       RENDERE
000400051014     c* 910       RICONSEGNARE STESSO INDIRIZZO
000500051014     c* 911       RICONSEGNARE STESSO INDIRIZZO CON APPUNTAMENTO (912,913)
000600051014     c* 914       RICONSEGNARE STESSO INDIRIZZO                  (915)
000700051014     c* 916       RICONSEGNARE - F.D.                            (917)
000800051014     c* 920       RICONSEGNARE ALTRO INDIRIZZO                   (921-924)
000900051014     c* IMPORTANTE: è indispensabile che l'associazione progressivo e
001000051014     c* testo di "testata" non venga modificato per evitare scarti in fase
001100051014     c* di ricezione disposizioni
001200051014     c* ATTENZIONE: ci possono essere dei problemi in fase di esecuzione
001300051014     c* del pgm se dovessero arrivare delle note generiche con progressivi
001400051014     c* > = 900
001500941014     H* FNLG95R1 *----------------------------------------------------*
001600921013     H*----------*                                                    *
001700941014     H*  RICEZIONE DISPOSIZIONI DAL MITTENTE (COPIA DA FNVAR A FNGNP) *
001800000214     H*---------------------------------------------------------------*
001900051108     FFNVARl1r  if   E           k DISK    infds(varinf) commit
002000051012     FFNVAR00r  Uf   E             DISK    commit  rename(fnvar000:fnvar)
002100050926     FFNVAR10S  O  A E             DISK    commit
002200050408     Ftignp00F  O    E             DISK    commit
002300050921     Ftignp01L  iF   E           K DISK
002400050309     F                                     RENAME(tignp000:tignp001)
002500050322     Ftigcp02L  UF   E           K DISK    commit
002600980130     FTABEL00F  IF   E           K DISK
002700050921     Fazorg01l  IF   E           K DISK
002800050921     Ffnblp01l  IF   E           K DISK
002900051115     FFiar901L  IF   E           K DISK
003000050921     Fcnaco00F  IF   E           K DISK
003100941014     D*---------------------------------------------------------------*
003200170222     D snot            S             50    DIM(2) CTDATA PERRCD(1)
003300941014     D*---------------------------------------------------------------*
003400050927     d esi             S              3    DIM(50)
003500050927     d vis             S              1    DIM(50)
003600050927     d XX              S              3  0
003700050930     d varinf          ds
003800050930     d  varnrr               397    400i 0
003900941014     D WLBDA8          DS
004000941014     D  G02DAT                 1      8  0
004100941014     D  G02INV                 9     16  0
004200941014     D  G02ERR                17     17
004300941014     D  G02TGI                18     22  0
004400941014     D                 DS
004500941014     D  DATCLI                21     28  0
004600941014     D  DATAGG                43     50  0
004700941014     D  NOTE                   1     50
004800050921     d Wdata8          DS
004900050921     d  dadata                 1      8  0
005000050921     d  adata                  9     16  0
005100050921     d  GioLav                17     21  0
005200051006     d dataiso         s               d   datfmt(*iso)
005300050928     D DS2n          E DS                  inz
005400050928     D DS3a          E DS                  inz
005500050928     D DS7q          E DS                  inz
005600050928     D OG143         E DS                  inz
005700050928     D DGCPFLR       E DS                  inz
005800050928     D DGNPDMC912    E DS                  prefix(d912) inz
005900050928     D DGNPDMC913    E DS                  prefix(d913) inz
006000050928     D DGNPDMC915    E DS                  prefix(d915) inz
006100050928     D DGNPDMC917    E DS                  prefix(d917) inz
006200050928     D DGNPDMC924    E DS                  prefix(d924) inz
006300170221     D DGNPDMC900    E DS                  prefix(d900) inz
006400170221     D DGNPDMC910    E DS                  prefix(d910) inz
006500170221     D DGNPDMC911    E DS                  prefix(d911) inz
006600170221     D DGNPDMC914    E DS                  prefix(d914) inz
006700170221     D DGNPDMC916    E DS                  prefix(d916) inz
006800170221     D DGNPDMC920    E DS                  prefix(d920) inz
006900050928     D kpjba         E DS
007000050922     D fnlg82ds      E DS
007100050928     D dsfnvar       E DS                  extname(fnvar00r) inz
007200050928     D dsfnvar£      E DS                  extname(fnvar00r) inz
007300050928     D                                     prefix(£)
007301171005      *ds per richiamo monitor variazioni
007302171005     d tigcp         e ds                  extname(tigcp00f)
007303171005     d fnlg33ds      e ds                  inz
007304171005     d tigcpa        e ds                  extname(tigcp00f)
007305171005     d                                     prefix(a)
007306171005     d tigcpb        e ds                  extname(tigcp00f)
007307171005     d                                     prefix(b)
007400050921     D DTAISO          S               D
007500921013     C*---------------------------------------------------------------*
007600051012     c     *start        setll     fnvarl1r
007700050322     c                   do        *hival
007800051108     c                   read(e)   fnvarl1r
007900051108     c                   if        %error
008000051108     c                   exsr      srfine
008100051108     c                   endif
008200051108     c                   if        %eof(fnvarl1r)
008300050322     c                   leave
008400050322     c                   endif
008500050921     c* rottura di giacenza
008600050921     C     VARAGC        IFNE      £varAGC
008700050921     C     VARNGC        ORNE      £varNGC
008800050921     C     VARFGC        ORNE      £varFGC
008900050921     c                   exsr      srctrgcp
009000050921     c                   end
009100050921     c* 13 allocata giacenza
009200050921     c                   if        *in13
009300121120      *** aggiunto il COMMIT qui per rilasciare il record.
009400121120     c                   COMMIT
009500050921     c                   iter
009600050921     c                   end
009700050922     c* testo della nota ind. 10 d'errore
009800050922     c                   if        not *in10
009900050930     C                   MOVEL     'GOK'         VARESI
010000050921     c                   exsr      srnota
010100050921     C                   ADD       1             GNPNPR
010200921013     C                   MOVEL     'R'           GNPTPN
010300051014     c                   if        gnpdmc <> ' '
010400050309     C                   WRITE     tignp000
010500050112     c                   end
010600051014     c                   end
010700050930     c* scrivo lo storico
010800050930     c                   exsr      srwtr
010900060126     c*    varnrr        chain     fnvar00r
011000060126     c*                  if        %found(fnvar00r)
011100060126     C     varnrr        DELETE    FNVAR
011200060126     c*                  end
011300050322     c                   enddo
011400060112     c* aggiorna ultima giacenza
011500050928     c                   if        £varngc <> 0 and not *in13 and not *in10
011600050921     c                   exsr      sragggcp
011700050921     c                   end
011800051018     c                   commit
011900050322     c                   seton                                        lr
012000941014     C**------------------------------------------------------------
012100050921     C** rottura di giacenza
012200941014     C**------------------------------------------------------------
012300050921     C     srctrgcp      BEGSR
012400170313     C                   clear                   utenteweb         1
012500050921     c* aggiorna tigcp00f e commit giacenza precedente
012600050928     c                   if        £varngc <> 0 and not *in13 and not *in10
012700050921     c                   exsr      sragggcp
012800060126     C* devo rileggere il record perchè il commit me l'ha rilasciato
012900060126     c     varnrr        chain(e)  fnvar00r
013000060126     c                   if        %error
013100060126     c                   exsr      srfine
013200060126     c                   endif
013300050921     c                   end
013400050921     c*
013500050921     c                   movel     dsfnvar       dsfnvar£
013600050921     C* VERIFICO ESISTENZA GIACENZA E BOLLA
013700050921     C     KGIA          CHAIN     tigcp02L                           1013
013800050921     c  n13              do
013900050921     C  N10GCPAAS        IFNE      VARAAS
014000050921     C     GCPLNP        ORNE      VARLNP
014100050921     C     GCPNRS        ORNE      VARNRS
014200050921     C     GCPNSP        ORNE      VARNSP
014300050921     C                   SETON                                        10
014400050921     C                   END
014500050921     C* Se la giacenza non esiste o non corrisponde la spediz.
014600050921     C* scrivo il file degli esiti con motivo INESISTENTE
014700050921     C     *IN10         IFEQ      *ON
014800050926     C                   MOVEL     'GSE'         VARESI
014900050921     C                   END
015000050921     C*
015100050921     C  N10              DO
015200050921     C  N10GCPFAS        COMP      25                                 10
015300050921     C  N10GCPDDM        COMP      0                                  10
015400050921     C* Se la giacenza è già stata trasmessa o è obbligatorio
015500050921     C* eseguire reso su riapertura oppure sono già state
015600050921     C* trasmesse le disposizioni all'arrivo scrivo errore nel
015700050921     C* fase degli esiti e memorizzo anche la relativa fase
015800050921     c                   if        *in10
015900050926     C                   MOVEL     'GFE'         VARESI
016000050921     C                   END
016100050921     C                   END
016200050921     C* MI POSIZIONO SULLE NOTE DELLA GIACENZA CON I DATI DI FNVAR00F
016300050921     C  N10              DO
016301171005     c                   eval      tigcpb = tigcp
016400050921     C                   Z-ADD     999           NPR
016500050921     C     KNOT          SETGT     tignp01L
016600050921     C                   READP     tignp01L                               12
016700050921     C     VARAGC        IFEQ      GNPAGC
016800050921     C     VARFGC        ANDEQ     GNPFGC
016900050921     C     VARNGC        ANDEQ     GNPNGC
017000050921     C     GNPFRG        ANDEQ     0
017100050921     C     GNPFAS        ANDEQ     20
017200050921     C                   ADD       1             GNPNPR
017300050921     C                   ELSE
017400050921     C                   Z-ADD     1             GNPNPR
017500050921     C                   Z-ADD     20            GNPFAS
017600050921     C                   Z-ADD     VARAGC        GNPAGC
017700050921     C                   Z-ADD     VARFGC        GNPFGC
017800050921     C                   Z-ADD     VARNGC        GNPNGC
017900050921     C                   Z-ADD     0             GNPFRG
018000050921     C                   END
018100170310      *verifica se disposizione mittenti o destinatario
018200170310     c                   select
018300170310     c                   when      varnpr = 900
018400170310     C                   MOVEL     VARDMC        DGNPDMC900
018500170310     C                   MOVEL     d900utenteweb utenteweb
018600170310     c                   when      varnpr = 910
018700170310     C                   MOVEL     VARDMC        DGNPDMC910
018800170310     C                   MOVEL     d910utenteweb utenteweb
018900170310     c                   when      varnpr = 911
019000170310     C                   MOVEL     VARDMC        DGNPDMC911
019100170310     C                   MOVEL     d911utenteweb utenteweb
019200170310     c                   when      varnpr = 914
019300170310     C                   MOVEL     VARDMC        DGNPDMC914
019400170310     C                   MOVEL     d914utenteweb utenteweb
019500170310     c                   when      varnpr = 916
019600170310     C                   MOVEL     VARDMC        DGNPDMC916
019700170310     C                   MOVEL     d916utenteweb utenteweb
019800170310     c                   when      varnpr = 920
019900170310     C                   MOVEL     VARDMC        DGNPDMC920
020000170310     C                   MOVEL     d920utenteweb utenteweb
020100170310     c                   endsl
020200050921     C* Scrittura nuova nota che riporta la data disposizioni del
020300050921     C* cliente e la data di aggioramento dell'archivio in sede
020400170222     c                   if        utenteweb  <> 'D'
020500170222     C                   MOVEA     snot(1)       NOTE
020600170222     c                   else
020700170222     C                   MOVEA     snot(2)       NOTE
020800170222     c                   endif
020900050921     C                   Z-ADD     VARDTR        DATCLI
021000050921     C                   Z-ADD     VARDRD        DATAGG
021100050921     C     DATAGG        IFEQ      0
021200050921     C                   Z-ADD     OGGI          DATAGG
021300050921     C                   END
021400050921     C                   Z-ADD     0             GNPDIF
021500050921     C                   Z-ADD     0             GNPNRI
021600050921     C                   MOVEL     NOTE          GNPDMC
021700050921     C                   MOVEL     'R'           GNPTPN
021800050921     C                   WRITE     tignp000
021900050921     C                   END
022000050921     C                   END
022100050921     C*
022200050921     c                   endsr
022300050921     C**------------------------------------------------------------
022400050921     C** aggiorna tigcp00f a seconda delle disposizioni ricevute
022500050921     C**------------------------------------------------------------
022600050921     C     sragggcp      BEGSR
022700050921     c* imposto per iniziare la fase 25
022800050921     c                   z-add     25            gcpfas
022900170310     C*                  clear                   utenteweb         1
023000170310     c*                  select
023100170310     c*                  when      £varnpr = 900
023200170310     C*                  MOVEL     £VARDMC       DGNPDMC900
023300170310     C*                  MOVEL     d900utenteweb utenteweb
023400170310     c*                  when      £varnpr = 910
023500170310     C*                  MOVEL     £VARDMC       DGNPDMC910
023600170310     C*                  MOVEL     d910utenteweb utenteweb
023700170310     c*                  when      £varnpr = 911
023800170310     C*                  MOVEL     £VARDMC       DGNPDMC911
023900170310     C*                  MOVEL     d911utenteweb utenteweb
024000170310     c*                  when      £varnpr = 914
024100170310     C*                  MOVEL     £VARDMC       DGNPDMC914
024200170310     C*                  MOVEL     d914utenteweb utenteweb
024300170310     c*                  when      £varnpr = 916
024400170310     C*                  MOVEL     £VARDMC       DGNPDMC916
024500170310     C*                  MOVEL     d916utenteweb utenteweb
024600170310     c*                  when      £varnpr = 920
024700170310     C*                  MOVEL     £VARDMC       DGNPDMC920
024800170310     C*                  MOVEL     d920utenteweb utenteweb
024900170310     c*                  endsl
025000050921     c* aggiorno giacenza con disposizioni via internet
025100170221     c                   if        (£varnpr = 900 and d900costante =
025200050928     c                             'RENDERE') OR
025300170221     c                             (£varnpr = 910 and d910costante =
025400050928     c                             'RICONSEGNARE STESSO INDIRIZZO') OR
025500170221     c                             (£varnpr = 911 and d911costante =
025600050928     c                             'RICONSEGNARE STESSO INDIRIZZO CON -
025700050928     c                             APPUNTAMENTO') OR
025800170221     c                             (£varnpr = 914 and d914costante =
025900050928     c                             'RICONSEGNARE STESSO INDIRIZZO') OR
026000170221     c                             (£varnpr = 916 and d916costante =
026100050928     c                             'RICONSEGNARE - F.D.') OR
026200170221     c                             (£varnpr = 920 and d920costante =
026300050928     c                             'RICONSEGNARE ALTRO INDIRIZZO')
026400050928     c                   exsr      srint
026500050921     c                   end
026600170222      * dopo controlli verifico se la disposizione è arrivata dal destinatario
026700170222      * forzo la fase a 31 per segnalare l'arrivo
026800170222     c                   if        utenteweb  = 'D'
026900170222     c                   z-add     31            gcpfas
027000170310     c                   eval      gcpdld = oggi
027100170222     c                   endif
027200050921     c                   update    tigcp000
027300050928     c                   commit
027301171005     c                   exsr      RegVariazioni
027400050921     C*
027500050921     c                   endsr
027600050921     C**------------------------------------------------------------
027700050927     C** scrive esito elaborazione disposizioni
027800050921     C**------------------------------------------------------------
027900050927     C     srwtr         BEGSR
028000050927     c* reperisco il flag se visualizzabile dal po s/n
028100050927     c                   z-add     1             xx
028200050927     c     varesi        lookup    esi(xx)                                01
028300050927     c                   if        *in01
028400050927     c                   movel     vis(xx)       varvis
028500050927     c                   else
028600050927     c                   clear                   varvis
028700050927     c                   end
028800050927     C*
028900050927     C                   Z-ADD     OGGI          VARDRD
029000050927     C                   Z-ADD     Ora           VARorz
029100050927     C                   WRITE     FNVARS10
029200050927     C*
029300050927     c                   endsr
029400050927     C**------------------------------------------------------------
029500050927     C** disposizioni da internet
029600050927     C**------------------------------------------------------------
029700050927     C     srint         BEGSR
029800050929     c* aggancio la spedizione originale per prelevare il cliente
029900051115     c                   z-add     0             ar9cas
030000051115     C     Kspep         CHAIN     Fiar901L
030100050929     c     kspep         chain     fnblp01l
030200050921     c                   if        %found(fnblp01l)
030300050921     C     BLPCCM        IFNE      *ZEROS
030400050921     C                   MOVE      blpccm        APPO              4 0
030500050928     C                   MOVE      blpccm        ACOKsC
030600050921     C                   ELSE
030700050921     C                   MOVE      blpksc        APPO              4 0
030800050928     C                   MOVE      blpKSC        ACOKsC
030900050921     C                   END
031000050929     c* pulisco la data eseguibilità e imposto la fase solo se non è
031100050929     c* consegna richiesta (914) perchè verranno
031200050929     c* impostati mentre elaboro il record 915 (La fase rimane a 25
031300050929     c* se se la consegna è > 3 gg lavorativi)
031400050921     c* se vario o non abilitato all'autoconferma
031500050921     c* lascio la fase 25 perchè la partenza deve confermare
031600050921     c* altrimenti metto la 30
031700050929     c                   if        £varnpr <> 914
031800050929     c                   clear                   gcpded
031900050930     c                   end
032000050930     c                   if        £varnpr <> 914 or
032100050930     c                             (£varnpr = 914 and giolav < 4)
032200050921     C     APPO          IFNE      8888
032300050921     C     APPO          ANDNE     9999
032400050921     c     KCLI          chain     cnaco00f
032500050928     c                   if        acorx1 = 00 and %found(cnaco00f)
032600050921     c                   eval      gcpfas = 30
032700050921     c                   end
032800050921     c                   end
032900050921     c                   end
033000050921     c                   end
033100050929     c                   if        gcplnp <> gcplnpa or
033200050929     c                             gcpaas <> gcpaasa or
033300050929     c                             gcpnrs <> gcpnrsa or
033400050929     c                             gcpnsp <> gcpnspa
033500050929     c     kspea         chain     fnblp01l
033600051115     c                   z-add     0             ar9cas
033700051115     C     Kspea         CHAIN     Fiar901L
033800050929     c                   end
033900050929     c* leggo tabella 3a, 7q
034000050921     c                   exsr      srtab
034100050921     c*
034200051107     c                   eval      gcpddm = oggi
034300050922     c* imposto la data immissione solo se autoconfermate altrimenti la
034400050922     c* lascia mettere dal pgm di immissione (conferma) disposizioni
034500050922     c                   if        gcpfas = 30
034600050928     c                   eval      gcpdld = oggi
034700050922     c                   else
034800050922     c                   clear                   gcpdld
034900050922     c                   end
035000050921     c* a questo punto imposto i campi a seconda della disposizione ricevuta
035100050921     c                   clear                   gcpnpc
035200050921     c                   eval      gcpvcs = 'N'
035300050921     c                   clear                   gcpcas
035400050921     c                   clear                   gcpvca
035500050921     c                   clear                   gcpppc
035600050921     c                   eval      gcpapc = 'N'
035700050928     C                   CLEAR                   GCPTFT
035800050928     C                   CLEAR                   GCPDSD
035900050928     C                   CLEAR                   GCPDSA
036000050928     C                   CLEAR                   GCPGGS
036100050928     C                   CLEAR                   GCPSGF
036200050928     C                   CLEAR                   GCPSGS
036300050928     C                   CLEAR                   GCPSGR
036400050928     C                   CLEAR                   GCPSGD
036500050928     C                   CLEAR                   GCPSGA
036600050928     C                   CLEAR                   GCPDIV
036700050928     C                   CLEAR                   GCPNBW
036800051013     c                   movel     gcpflr        dgcpflr
036900051014     c                   eval      D§GCPdisa = 'S'
037000170222     c                   eval      D§GCPUTEW = utenteweb
037100051013     c                   movel     dgcpflr       gcpflr
037200050921     c* RESO
037300050921     c                   if        £varnpr = 900
037400050921     c                   eval      gcpdis = 'R'
037500050921     c                   eval      gcppsg = 'M'
037600060808      * se non è già stato forzato lo imposto
037700060808     c                   if        gcpasg = *blank
037800050921     c                   eval      gcpasg = 'S'
037900060808     c                   endif
038000050922     c                   eval      gcptft = 9
038100050921     c* se autoconfermato chiamo pgm x calcolo spese
038200050921     c                   if        gcpfas = 30
038300060112     c                   update    tigcp000
038400050922     c                   clear                   fnlg82ds
038500050922     c                   move      gcpagc        d82agc
038600050922     c                   move      gcpfgc        d82fgc
038700050922     c                   move      gcpngc        d82ngc
038800050922     c                   move      gcpfrg        d82frg
038900050922     c                   eval      d82nodsp = 'X'
039000050922     c                   eval      d82comm = 'N'
039100050922     c                   eval      d82tri = 'D'
039200050922     c                   movel(p)  fnlg82ds      kpjbu
039300050922     c                   call      'FNLG14R'
039400050922     c                   parm                    kpjba
039500051021     c     kgia1         chain     tigcp02l
039600050928     c* se ritorno con errore lascio la fase 25 in modo che la partenza
039700050928     c* possa agire
039800050928     c                   if        d82f12 <> ' '
039900050928     c                   z-add     25            gcpfas
040000050921     c                   end
040001171005     c                   exsr      RegVariazioni
040100050928     c                   end
040200050922     c                   end
040300050921     c* CONSEGNA
040400050921     c                   if        £varnpr > 900
040500050921     c                   eval      gcpdis = 'C'
040600050921     c* appuntamento
040700050921     c                   if        £varnpr = 911
040800050921     c                   movel     gcpflr        dgcpflr
040900050921     c                   eval      D§GCPAPP = 'A'
041000050921     c                   movel     dgcpflr       gcpflr
041100050921     c                   end
041200050929     c* spese giacenza
041300050921     c                   exsr      srspecon
041400050929     c* provvigioni c/ass.
041500050929     c                   exsr      srprovca
041600050921     c                   end
041700050921     c                   endsr
041701171005     C*__________________________________________________________
041702171005     c     RegVariazioni begsr
041703171005     C*__________________________________________________________
041704171005     c                   eval      ILG33CTA = 'M'
041705171005     c                   eval      ILG33PRU = knmus
041706171005     c                   eval      ILG33NOJ = knmeb
041707171005     c                   eval      ILG33PGM = 'FNLG95R1'
041708171005     c                   eval      tigcpa = tigcp
041709171005     c                   call      'FNLG33R'
041710171005     c                   parm                    fnlg33ds
041711171005     c                   parm                    tigcpb
041712171005     c                   parm                    tigcpa
041713171005     c                   endsr
041800050921     C**------------------------------------------------------------
041900050921     C** calcola flag spese
042000050921     C**------------------------------------------------------------
042100050921     C     srspecon      BEGSR
042200050921     C* PROPONGO LA PARTICOLARITA NO SPESE
042300060808     C* solo se non è già stato forzato
042400060808     c                   if        gcpasg = *blank
042500050928     C                   IF        (§7Q1SF = 'N' and
042600050928     c                             %subst(§3ATB1: 1: 1)='F') or
042700050928     C                             (§7Q1SA = 'N' and
042800050928     c                             %subst(§3ATB1: 1: 1)='A')
042900050921     C                   MOVEL     'N'           gcpASG
043000050921     C                   ELSE
043100050921     C* ALTRIMENTI SEMPRE SI SPESE
043200050921     C                   MOVEL     'S'           gcpASG
043300050921     C                   END
043400060808     C                   Endif
043500050921     C*  PROPONGO LO STANDARD  FRANCHI  -> MITT.
043600050921     C*                        ASSEGNATI-> DEST.
043700050921     C*  O QUANTO PREVISTO DALLA PARTICOLARITA
043800050929     C                   MOVEL     *BLANKS       gcpPSG
043900050921     C     gcpASG        IFEQ      'S'
044000050928      * FRANCO
044100050928     c                   if        %subst(§3ATB1: 1: 1)='F' or
044200050928     c* internaz.
044300050928     c                             giaest = 'E'
044400050921      * standard
044500050921     C                   MOVEL     'M'           gcpPSG
044600050921      * dest.
044700050921     C     §7Q1SF        IFEQ      'D'
044800050921     C                   MOVEL     'D'           gcpPSG
044900050921     C                   END
045000050921     C                   END
045100050928      * ASSEGNATO
045200050928     c                   if        %subst(§3ATB1: 1: 1)='A'
045300050928      * internaz.
045400050928     c                   if        giaest <> 'E'
045500050921      * standard
045600050921     C                   MOVEL     'D'           gcpPSG
045700050921      * mittente
045800050921     C     §7Q1SA        IFEQ      'M'
045900050921     C                   MOVEL     'M'           gcpPSG
046000050921     C                   END
046100050928     c* Per export no addebito spese giacenza al destinatario ???
046200050928     c                   else
046300050928     C                   MOVEL     'M'           gcpPSG
046400050921     C                   END
046500050929     C                   END
046600050929     C                   END
046700050921     c*
046800050921     c                   endsr
046900050929     C**------------------------------------------------------------
047000050929     C** provvigioni c/assegno
047100050929     C**------------------------------------------------------------
047200050929     C     srprovca      BEGSR
047300050929     c*
047400050929     c                   eval      gcpapc = 'N'
047500050929     c                   clear                   gcpppc
047600051115     c                   if        §3afca <> 0 and ar9cas <> 0
047700050929     c                   eval      gcpapc = 'S'
047800050929     c* contrassegno sulla seconda bolla. se A3 destinatario altrimenti
047900050929     c* stesso ragionamento come se fosse sulla 1° bolla
048000050929     c                   if        §3afca = 2
048100050929     c                   if        §3atb2 = 'A3'
048200050929     c                   eval      gcpppc = 'D'
048300050929     c                   else
048400050929     c                   eval      §3afca = 1
048500050929     c                   end
048600050929     c                   end
048700050929     c* contrassegno sulla prima bolla. se Franco provv. mittente se
048800050929     c* assegnato prov. destinatario
048900050929     c                   if        §3afca = 1
049000050929     c                   if        %subst(§3ATB1: 1: 1)='F'
049100050929     c                   eval      gcpppc = 'M'
049200050929     c                   else
049300050929     c                   eval      gcpppc = 'D'
049400050929     c                   end
049500050929     c                   end
049600050929     c                   end
049700050929     c                   endsr
049800050921     C**------------------------------------------------------------
049900050921     C** data consegna richiesta
050000050921     C**------------------------------------------------------------
050100050921     C     srdcr         BEGSR
050200051011     C* tolgo un gg alla data immessa per data eseguibilità solo se scelto
050300051011     c* IN DATA altrimenti se scelto DOPO IL lascio la data immessa
050400051011     c                   move      d915ndcr      dataiso
050500051011     c                   if        %subst(d915costante: 1: 7)='IN DATA'
050600051011     c                   move      dataiso       data915           8 0
050700051125     c                   if        data915 > oggi
050800051011     c                   move      dataiso       dadata
050900051011     c                   move      dataiso       adata
051000051006     c                   CALL      'XSRLAV8'
051100051006     c                   PARM                    Wdata8
051200051006     c                   z-add     giolav        giolavs           1 0
051300051006    3c                   do        *hival
051400051006     c     dataiso       subdur    1:*d          dataiso
051500051006     c                   move      dataiso       dadata
051600051011     c                   move      data915       adata
051700051006     c                   CALL      'XSRLAV8'
051800051006     c                   PARM                    Wdata8
051900051006    4c                   if        giolav = 0 or giolav > giolavs
052000051006     c                   leave
052100051006    4c                   endif
052200051006    3c                   enddo
052300051125     c                   end
052400051014     c                   else
052500051014     c* per riconoscere se IN DATA metto 915 nel campo GNPDIF se DOPO IL
052600051014     c* metto 9152
052700051014     c                   z-add     9152          gnpdif
052800051011     c                   end
052900051006     c                   move      dataiso       gcpded
053000051107     c* se i gg lavorativi che intercorrono dalla data eseguibilità ( la
053100051107     c* data consegna richiesta) e la data in cui riceviamo le disposizioni
053200051107     c* sono maggiori di 3 rimetto la fase a 25
053300050921     c* in modo che la partenza la conferma
053400051107     c                   move      oggi          dadata
053500051011     c                   move      dataiso       adata
053600051006     c                   CALL      'XSRLAV8'
053700051006     c                   PARM                    Wdata8
053800050921    4c                   if        giolav > 3
053900050921     c                   eval      gcpfas = 25
054000050921    4c                   endif
054100050921     c*
054200050921     c                   endsr
054300050921     C**------------------------------------------------------------
054400050921     C** imposto il testo della nota
054500050921     C**------------------------------------------------------------
054600050921     C     srnota        BEGSR
054700050928     C                   CLEAR                   GCPDED
054800050921     c* note libere o nuovo indirizzo
054900050921     C                   MOVEL     VARDMC        GNPDMC
055000051013     c* memorizzo il progressivo nota nel numero fax in modo da poter
055100051013     c* riconoscere il tipo disposizione quando leggerò il file TIGNP00F
055200051013     c                   if        varnpr >= 900
055300051013     c                   z-add     varnpr        gnpdif
055400051013     c                   end
055500050921     c                   select
055600051013     c                   when      varnpr = 912
055700050921     C                   MOVEL     VARDMC        DGNPDMC912
055800051014     c                   eval      gnpdmc = d912costante + d912telefono
055900050921     c                   when      varnpr = 913
056000050921     C                   MOVEL     VARDMC        DGNPDMC913
056100051014     c                   eval      gnpdmc = d913costante + d913persona
056200050921     c                   when      varnpr = 915
056300050921     C                   MOVEL     VARDMC        DGNPDMC915
056400050921     c                   exsr      srdcr
056500050921     c                   movel     d915ndcr      com10            10
056600051014     c                   eval      gnpdmc = d915costante + com10
056700050921     c                   when      varnpr = 917
056800050921     C                   MOVEL     VARDMC        DGNPDMC917
056900051014     c                   eval      gnpdmc = d917costante + d917persona
057000050921     c                   when      varnpr = 924
057100050921     C                   MOVEL     VARDMC        DGNPDMC924
057200051014     c                   eval      gnpdmc = d924cap + d924loc +
057300051014     c                             d924pro +  d924naz
057400050921     c                   endsl
057500050921     c*
057600050921     c                   endsr
057700050921     C**------------------------------------------------------------
057800050921     C** aggancio tabelle neccessarie
057900050921     C**------------------------------------------------------------
058000050921     C     srtab         BEGSR
058100050921     C* TIPO BOLLA
058200050921     C                   MOVEL     '3A'          TBLCOD
058300050921     C                   MOVEL(p)  blpCBO        TBLKEY
058400050921     c                   clear                   ds3a
058500050921     C     KTAB          CHAIN     TABEL00f
058600050921     C                   if        %found(tabel00f)
058700050921     C                   MOVEL     TBLUNI        DS3A
058800050921     c                   end
058900050921     C* Carico particolarità
059000050921     C                   CLEAR                   DS7Q
059100050921     C     BLPGGA        IFNE      *BLANKS
059200050921     C                   MOVEL     '7Q'          TBLCOD
059300050921     C                   MOVEL(p)  blpgga        TBLKEY
059400050921     C     KTAB          CHAIN     TABEL00f
059500050921     C                   if        %found(tabel00f)
059600050921     C                   MOVEL     TBLUNI        DS7Q
059700050921     C                   END
059800050921     C                   END
059900050921     C* FIL. CHE HA APERTO LA GIACENZA
060000050921     C                   clear                   OG143
060100050921     C                   MOVEL     *blank        GIAEST            1
060200050921     C     gcpfgc        CHAIN     AZORG01L
060300050921     C                   if        %found(azorg01l) and orgfva <> '*'
060400050921     C                   MOVEL     ORGDE3        OG143
060500050921     C     §OgNTW        ifeq      'FED'
060600050921     C     §OgNTW        oreq      'EEX'
060700050921     C     §OgNTW        oreq      'EUP'
060800050921     C                   MOVEL     'E'           GIAEST
060900050921     c                   end
061000050921     C                   END
061100050921     c*
061200050921     c                   endsr
061300051108     C**************************************************************************
061400051108     C* esco con codice d'errore
061500051108     C**************************************************************************
061600051108     C     srfine        BEGSR
061700051108     c*
061800051108     c                   rolbk
061900051108     c* record allocato
062000051108     c                   if        %status = 1218
062100051108     c                   eval      kpjbu = '2'
062200051108     c                   else
062300051108     c                   eval      kpjbu = '1'
062400051108     c                   end
062500051108     c                   seton                                        lr
062600051108     c                   return
062700051108     c*
062800051108     c                   endsr
062900050928     C**------------------------------------------------------------
063000050928     C** OPERAZIONI INIZIALI
063100050928     C**------------------------------------------------------------
063200050928     C     *INZSR        BEGSR
063300050922     c*
063400050922     C     *ENTRY        PLIST
063500050922     C                   PARM                    KPJBA
063600050921     C*
063700050926     C                   Z-ADD     1             CODUT             1 0
063800050921     C                   Z-ADD     1             ACOKUT
063900050921     C                   Z-ADD     151           ACOKCC
064000980130     C* DEFINIZIONE CHIAVI
064100941014     C     KGIA          KLIST
064200941014     C                   KFLD                    VARAGC
064300941014     C                   KFLD                    VARFGC
064400941014     C                   KFLD                    VARNGC
064500990628     C                   KFLD                    FRG               2 0
064600941014     C                   Z-ADD     0             FRG
064700051021     C     KGIA1         KLIST
064800051021     C                   KFLD                    £VARAGC
064900051021     C                   KFLD                    £VARFGC
065000051021     C                   KFLD                    £VARNGC
065100051021     C                   KFLD                    FRG               2 0
065200941014     C*
065300941014     C     KNOT          KLIST
065400941014     C                   KFLD                    VARAGC
065500941014     C                   KFLD                    VARFGC
065600941014     C                   KFLD                    VARNGC
065700941014     C                   KFLD                    FRG
065800941014     C                   KFLD                    FAS               3 0
065900941014     C                   KFLD                    NPR               3 0
066000980130     C*
066100980130     C     KTAB          KLIST
066200980130     C                   KFLD                    CODUT
066300050921     C                   KFLD                    TBLCOD
066400050921     C                   KFLD                    TBLKEY
066500050927     C     KTAB1         KLIST
066600050927     C                   KFLD                    CODUT
066700050927     C                   KFLD                    TBLCOD
066800050921     C*
066900050921     C     KCLI          KLIST
067000050921     C                   KFLD                    ACOKUT
067100050921     C                   KFLD                    ACOKCC
067200050921     C                   KFLD                    ACOKSC
067300050921     C*
067400050929     C     Kspep         KLIST
067500050929     C                   KFLD                    gcpaas
067600050929     C                   KFLD                    gcplnp
067700050929     C                   KFLD                    gcpnrs
067800050929     C                   KFLD                    gcpnsp
067900980130     C*
068000050929     C     Kspea         KLIST
068100050929     C                   KFLD                    gcpaasa
068200050929     C                   KFLD                    gcplnpa
068300050929     C                   KFLD                    gcpnrsa
068400050929     C                   KFLD                    gcpnspa
068500050929     C*
068600941014     C                   Z-ADD     20            FAS
068700941014     C                   Z-ADD     999           NPR
068800941014     C*  Reperisco data del giorno
068900941014     C                   CLEAR                   WLBDA8
069000941014     C                   TIME                    WHHDAT           14 0
069100941014     C                   MOVE      WHHDAT        G02DAT
069200941014     C                   Z-ADD     0             G02INV
069300941014     C                   MOVEL     ' '           G02ERR
069400941014     C                   CALL      'XSRDA8'
069500941014     C                   PARM                    WLBDA8
069600941014     C                   Z-ADD     G02INV        OGGI              8 0
069700050926     C                   MOVEl     WHHDAT        ora               6 0
069800050927     c* carico tabella 2N x esito elaborazione
069900050927     c                   clear                   ds2n
070000050927     c                   clear                   xx
070100050927     c                   eval      tblcod = '2N'
070200050927     C     KTAB1         setll     TABEL00f
070300050927     c                   do        *hival
070400050927     C     KTAB1         reade     TABEL00f
070500050927     C                   if        %eof(tabel00f)
070600050927     c                   leave
070700050927     c                   end
070800050927     c                   if        tblflg <> ' '
070900050927     c                   iter
071000050927     c                   end
071100050927     C                   MOVEL     TBLUNI        DS2n
071200050927     c                   add       1             xx
071300050928     c                   movel     tblkey        esi(xx)
071400050927     c                   movel     §2Nvis        vis(xx)
071500050927     c                   enddo
071600941014     C*
071700941014     C                   ENDSR
071800050112**         snot
071900941014DATA DISPOS.CLIENTE:XXXXXXXX DATA AGGIOR.:XXXXXXXX
072000170222DATA DISPOS.DESTIN.:XXXXXXXX DATA AGGIOR.:XXXXXXXX
