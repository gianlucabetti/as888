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
003200050112     D snot            S              1    DIM(50) CTDATA PERRCD(50)
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
006300050928     D kpjba         E DS
006400050922     D fnlg82ds      E DS
006500050928     D dsfnvar       E DS                  extname(fnvar00r) inz
006600050928     D dsfnvar£      E DS                  extname(fnvar00r) inz
006700050928     D                                     prefix(£)
006800050921     D DTAISO          S               D
006900921013     C*---------------------------------------------------------------*
007000051012     c     *start        setll     fnvarl1r
007100050322     c                   do        *hival
007200051108     c                   read(e)   fnvarl1r
007300051108     c                   if        %error
007400051108     c                   exsr      srfine
007500051108     c                   endif
007600051108     c                   if        %eof(fnvarl1r)
007700050322     c                   leave
007800050322     c                   endif
007900050921     c* rottura di giacenza
008000050921     C     VARAGC        IFNE      £varAGC
008100050921     C     VARNGC        ORNE      £varNGC
008200050921     C     VARFGC        ORNE      £varFGC
008300050921     c                   exsr      srctrgcp
008400050921     c                   end
008500050921     c* 13 allocata giacenza
008600050921     c                   if        *in13
008700121120      *** aggiunto il COMMIT qui per rilasciare il record.
008800121120     c                   COMMIT
008900050921     c                   iter
009000050921     c                   end
009100050922     c* testo della nota ind. 10 d'errore
009200050922     c                   if        not *in10
009300050930     C                   MOVEL     'GOK'         VARESI
009400050921     c                   exsr      srnota
009500050921     C                   ADD       1             GNPNPR
009600921013     C                   MOVEL     'R'           GNPTPN
009700051014     c                   if        gnpdmc <> ' '
009800050309     C                   WRITE     tignp000
009900050112     c                   end
010000051014     c                   end
010100050930     c* scrivo lo storico
010200050930     c                   exsr      srwtr
010300060126     c*    varnrr        chain     fnvar00r
010400060126     c*                  if        %found(fnvar00r)
010500060126     C     varnrr        DELETE    FNVAR
010600060126     c*                  end
010700050322     c                   enddo
010800060112     c* aggiorna ultima giacenza
010900050928     c                   if        £varngc <> 0 and not *in13 and not *in10
011000050921     c                   exsr      sragggcp
011100050921     c                   end
011200051018     c                   commit
011300050322     c                   seton                                        lr
011400941014     C**------------------------------------------------------------
011500050921     C** rottura di giacenza
011600941014     C**------------------------------------------------------------
011700050921     C     srctrgcp      BEGSR
011800050921     c* aggiorna tigcp00f e commit giacenza precedente
011900050928     c                   if        £varngc <> 0 and not *in13 and not *in10
012000050921     c                   exsr      sragggcp
012100060126     C* devo rileggere il record perchè il commit me l'ha rilasciato
012200060126     c     varnrr        chain(e)  fnvar00r
012300060126     c                   if        %error
012400060126     c                   exsr      srfine
012500060126     c                   endif
012600050921     c                   end
012700050921     c*
012800050921     c                   movel     dsfnvar       dsfnvar£
012900050921     C* VERIFICO ESISTENZA GIACENZA E BOLLA
013000050921     C     KGIA          CHAIN     tigcp02L                           1013
013100050921     c  n13              do
013200050921     C  N10GCPAAS        IFNE      VARAAS
013300050921     C     GCPLNP        ORNE      VARLNP
013400050921     C     GCPNRS        ORNE      VARNRS
013500050921     C     GCPNSP        ORNE      VARNSP
013600050921     C                   SETON                                        10
013700050921     C                   END
013800050921     C* Se la giacenza non esiste o non corrisponde la spediz.
013900050921     C* scrivo il file degli esiti con motivo INESISTENTE
014000050921     C     *IN10         IFEQ      *ON
014100050926     C                   MOVEL     'GSE'         VARESI
014200050921     C                   END
014300050921     C*
014400050921     C  N10              DO
014500050921     C  N10GCPFAS        COMP      25                                 10
014600050921     C  N10GCPDDM        COMP      0                                  10
014700050921     C* Se la giacenza è già stata trasmessa o è obbligatorio
014800050921     C* eseguire reso su riapertura oppure sono già state
014900050921     C* trasmesse le disposizioni all'arrivo scrivo errore nel
015000050921     C* fase degli esiti e memorizzo anche la relativa fase
015100050921     c                   if        *in10
015200050926     C                   MOVEL     'GFE'         VARESI
015300050921     C                   END
015400050921     C                   END
015500050921     C* MI POSIZIONO SULLE NOTE DELLA GIACENZA CON I DATI DI FNVAR00F
015600050921     C  N10              DO
015700050921     C                   Z-ADD     999           NPR
015800050921     C     KNOT          SETGT     tignp01L
015900050921     C                   READP     tignp01L                               12
016000050921     C     VARAGC        IFEQ      GNPAGC
016100050921     C     VARFGC        ANDEQ     GNPFGC
016200050921     C     VARNGC        ANDEQ     GNPNGC
016300050921     C     GNPFRG        ANDEQ     0
016400050921     C     GNPFAS        ANDEQ     20
016500050921     C                   ADD       1             GNPNPR
016600050921     C                   ELSE
016700050921     C                   Z-ADD     1             GNPNPR
016800050921     C                   Z-ADD     20            GNPFAS
016900050921     C                   Z-ADD     VARAGC        GNPAGC
017000050921     C                   Z-ADD     VARFGC        GNPFGC
017100050921     C                   Z-ADD     VARNGC        GNPNGC
017200050921     C                   Z-ADD     0             GNPFRG
017300050921     C                   END
017400050921     C* Scrittura nuova nota che riporta la data disposizioni del
017500050921     C* cliente e la data di aggioramento dell'archivio in sede
017600050921     C                   Z-ADD     VARDTR        DATCLI
017700050921     C                   Z-ADD     VARDRD        DATAGG
017800050921     C     DATAGG        IFEQ      0
017900050921     C                   Z-ADD     OGGI          DATAGG
018000050921     C                   END
018100050921     C                   Z-ADD     0             GNPDIF
018200050921     C                   Z-ADD     0             GNPNRI
018300050921     C                   MOVEL     NOTE          GNPDMC
018400050921     C                   MOVEL     'R'           GNPTPN
018500050921     C                   WRITE     tignp000
018600050921     C                   END
018700050921     C                   END
018800050921     C*
018900050921     c                   endsr
019000050921     C**------------------------------------------------------------
019100050921     C** aggiorna tigcp00f a seconda delle disposizioni ricevute
019200050921     C**------------------------------------------------------------
019300050921     C     sragggcp      BEGSR
019400050921     c* imposto per iniziare la fase 25
019500050921     c                   z-add     25            gcpfas
019600050921     c* aggiorno giacenza con disposizioni via internet
019700050928     c                   if        (£varnpr = 900 and £vardmc =
019800050928     c                             'RENDERE') OR
019900050928     c                             (£varnpr = 910 and £vardmc =
020000050928     c                             'RICONSEGNARE STESSO INDIRIZZO') OR
020100050928     c                             (£varnpr = 911 and £vardmc =
020200050928     c                             'RICONSEGNARE STESSO INDIRIZZO CON -
020300050928     c                             APPUNTAMENTO') OR
020400050928     c                             (£varnpr = 914 and £vardmc =
020500050928     c                             'RICONSEGNARE STESSO INDIRIZZO') OR
020600050928     c                             (£varnpr = 916 and £vardmc =
020700050928     c                             'RICONSEGNARE - F.D.') OR
020800050928     c                             (£varnpr = 920 and £vardmc =
020900050928     c                             'RICONSEGNARE ALTRO INDIRIZZO')
021000050928     c                   exsr      srint
021100050921     c                   end
021200050921     c                   update    tigcp000
021300050928     c                   commit
021400050921     C*
021500050921     c                   endsr
021600050921     C**------------------------------------------------------------
021700050927     C** scrive esito elaborazione disposizioni
021800050921     C**------------------------------------------------------------
021900050927     C     srwtr         BEGSR
022000050927     c* reperisco il flag se visualizzabile dal po s/n
022100050927     c                   z-add     1             xx
022200050927     c     varesi        lookup    esi(xx)                                01
022300050927     c                   if        *in01
022400050927     c                   movel     vis(xx)       varvis
022500050927     c                   else
022600050927     c                   clear                   varvis
022700050927     c                   end
022800050927     C*
022900050927     C                   Z-ADD     OGGI          VARDRD
023000050927     C                   Z-ADD     Ora           VARorz
023100050927     C                   WRITE     FNVARS10
023200050927     C*
023300050927     c                   endsr
023400050927     C**------------------------------------------------------------
023500050927     C** disposizioni da internet
023600050927     C**------------------------------------------------------------
023700050927     C     srint         BEGSR
023800050929     c* aggancio la spedizione originale per prelevare il cliente
023900051115     c                   z-add     0             ar9cas
024000051115     C     Kspep         CHAIN     Fiar901L
024100050929     c     kspep         chain     fnblp01l
024200050921     c                   if        %found(fnblp01l)
024300050921     C     BLPCCM        IFNE      *ZEROS
024400050921     C                   MOVE      blpccm        APPO              4 0
024500050928     C                   MOVE      blpccm        ACOKsC
024600050921     C                   ELSE
024700050921     C                   MOVE      blpksc        APPO              4 0
024800050928     C                   MOVE      blpKSC        ACOKsC
024900050921     C                   END
025000050929     c* pulisco la data eseguibilità e imposto la fase solo se non è
025100050929     c* consegna richiesta (914) perchè verranno
025200050929     c* impostati mentre elaboro il record 915 (La fase rimane a 25
025300050929     c* se se la consegna è > 3 gg lavorativi)
025400050921     c* se vario o non abilitato all'autoconferma
025500050921     c* lascio la fase 25 perchè la partenza deve confermare
025600050921     c* altrimenti metto la 30
025700050929     c                   if        £varnpr <> 914
025800050929     c                   clear                   gcpded
025900050930     c                   end
026000050930     c                   if        £varnpr <> 914 or
026100050930     c                             (£varnpr = 914 and giolav < 4)
026200050921     C     APPO          IFNE      8888
026300050921     C     APPO          ANDNE     9999
026400050921     c     KCLI          chain     cnaco00f
026500050928     c                   if        acorx1 = 00 and %found(cnaco00f)
026600050921     c                   eval      gcpfas = 30
026700050921     c                   end
026800050921     c                   end
026900050921     c                   end
027000050921     c                   end
027100050929     c                   if        gcplnp <> gcplnpa or
027200050929     c                             gcpaas <> gcpaasa or
027300050929     c                             gcpnrs <> gcpnrsa or
027400050929     c                             gcpnsp <> gcpnspa
027500050929     c     kspea         chain     fnblp01l
027600051115     c                   z-add     0             ar9cas
027700051115     C     Kspea         CHAIN     Fiar901L
027800050929     c                   end
027900050929     c* leggo tabella 3a, 7q
028000050921     c                   exsr      srtab
028100050921     c*
028200051107     c                   eval      gcpddm = oggi
028300050922     c* imposto la data immissione solo se autoconfermate altrimenti la
028400050922     c* lascia mettere dal pgm di immissione (conferma) disposizioni
028500050922     c                   if        gcpfas = 30
028600050928     c                   eval      gcpdld = oggi
028700050922     c                   else
028800050922     c                   clear                   gcpdld
028900050922     c                   end
029000050921     c* a questo punto imposto i campi a seconda della disposizione ricevuta
029100050921     c                   clear                   gcpnpc
029200050921     c                   eval      gcpvcs = 'N'
029300050921     c                   clear                   gcpcas
029400050921     c                   clear                   gcpvca
029500050921     c                   clear                   gcpppc
029600050921     c                   eval      gcpapc = 'N'
029700050928     C                   CLEAR                   GCPTFT
029800050928     C                   CLEAR                   GCPDSD
029900050928     C                   CLEAR                   GCPDSA
030000050928     C                   CLEAR                   GCPGGS
030100050928     C                   CLEAR                   GCPSGF
030200050928     C                   CLEAR                   GCPSGS
030300050928     C                   CLEAR                   GCPSGR
030400050928     C                   CLEAR                   GCPSGD
030500050928     C                   CLEAR                   GCPSGA
030600050928     C                   CLEAR                   GCPDIV
030700050928     C                   CLEAR                   GCPNBW
030800051013     c                   movel     gcpflr        dgcpflr
030900051014     c                   eval      D§GCPdisa = 'S'
031000051013     c                   movel     dgcpflr       gcpflr
031100050921     c* RESO
031200050921     c                   if        £varnpr = 900
031300050921     c                   eval      gcpdis = 'R'
031400050921     c                   eval      gcppsg = 'M'
031500060808      * se non è già stato forzato lo imposto
031600060808     c                   if        gcpasg = *blank
031700050921     c                   eval      gcpasg = 'S'
031800060808     c                   endif
031900050922     c                   eval      gcptft = 9
032000050921     c* se autoconfermato chiamo pgm x calcolo spese
032100050921     c                   if        gcpfas = 30
032200060112     c                   update    tigcp000
032300050922     c                   clear                   fnlg82ds
032400050922     c                   move      gcpagc        d82agc
032500050922     c                   move      gcpfgc        d82fgc
032600050922     c                   move      gcpngc        d82ngc
032700050922     c                   move      gcpfrg        d82frg
032800050922     c                   eval      d82nodsp = 'X'
032900050922     c                   eval      d82comm = 'N'
033000050922     c                   eval      d82tri = 'D'
033100050922     c                   movel(p)  fnlg82ds      kpjbu
033200050922     c                   call      'FNLG14R'
033300050922     c                   parm                    kpjba
033400051021     c     kgia1         chain     tigcp02l
033500050928     c* se ritorno con errore lascio la fase 25 in modo che la partenza
033600050928     c* possa agire
033700050928     c                   if        d82f12 <> ' '
033800050928     c                   z-add     25            gcpfas
033900050921     c                   end
034000050928     c                   end
034100050922     c                   end
034200050921     c* CONSEGNA
034300050921     c                   if        £varnpr > 900
034400050921     c                   eval      gcpdis = 'C'
034500050921     c* appuntamento
034600050921     c                   if        £varnpr = 911
034700050921     c                   movel     gcpflr        dgcpflr
034800050921     c                   eval      D§GCPAPP = 'A'
034900050921     c                   movel     dgcpflr       gcpflr
035000050921     c                   end
035100050929     c* spese giacenza
035200050921     c                   exsr      srspecon
035300050929     c* provvigioni c/ass.
035400050929     c                   exsr      srprovca
035500050921     c                   end
035600050921     c                   endsr
035700050921     C**------------------------------------------------------------
035800050921     C** calcola flag spese
035900050921     C**------------------------------------------------------------
036000050921     C     srspecon      BEGSR
036100050921     C* PROPONGO LA PARTICOLARITA NO SPESE
036200060808     C* solo se non è già stato forzato
036300060808     c                   if        gcpasg = *blank
036400050928     C                   IF        (§7Q1SF = 'N' and
036500050928     c                             %subst(§3ATB1: 1: 1)='F') or
036600050928     C                             (§7Q1SA = 'N' and
036700050928     c                             %subst(§3ATB1: 1: 1)='A')
036800050921     C                   MOVEL     'N'           gcpASG
036900050921     C                   ELSE
037000050921     C* ALTRIMENTI SEMPRE SI SPESE
037100050921     C                   MOVEL     'S'           gcpASG
037200050921     C                   END
037300060808     C                   Endif
037400050921     C*  PROPONGO LO STANDARD  FRANCHI  -> MITT.
037500050921     C*                        ASSEGNATI-> DEST.
037600050921     C*  O QUANTO PREVISTO DALLA PARTICOLARITA
037700050929     C                   MOVEL     *BLANKS       gcpPSG
037800050921     C     gcpASG        IFEQ      'S'
037900050928      * FRANCO
038000050928     c                   if        %subst(§3ATB1: 1: 1)='F' or
038100050928     c* internaz.
038200050928     c                             giaest = 'E'
038300050921      * standard
038400050921     C                   MOVEL     'M'           gcpPSG
038500050921      * dest.
038600050921     C     §7Q1SF        IFEQ      'D'
038700050921     C                   MOVEL     'D'           gcpPSG
038800050921     C                   END
038900050921     C                   END
039000050928      * ASSEGNATO
039100050928     c                   if        %subst(§3ATB1: 1: 1)='A'
039200050928      * internaz.
039300050928     c                   if        giaest <> 'E'
039400050921      * standard
039500050921     C                   MOVEL     'D'           gcpPSG
039600050921      * mittente
039700050921     C     §7Q1SA        IFEQ      'M'
039800050921     C                   MOVEL     'M'           gcpPSG
039900050921     C                   END
040000050928     c* Per export no addebito spese giacenza al destinatario ???
040100050928     c                   else
040200050928     C                   MOVEL     'M'           gcpPSG
040300050921     C                   END
040400050929     C                   END
040500050929     C                   END
040600050921     c*
040700050921     c                   endsr
040800050929     C**------------------------------------------------------------
040900050929     C** provvigioni c/assegno
041000050929     C**------------------------------------------------------------
041100050929     C     srprovca      BEGSR
041200050929     c*
041300050929     c                   eval      gcpapc = 'N'
041400050929     c                   clear                   gcpppc
041500051115     c                   if        §3afca <> 0 and ar9cas <> 0
041600050929     c                   eval      gcpapc = 'S'
041700050929     c* contrassegno sulla seconda bolla. se A3 destinatario altrimenti
041800050929     c* stesso ragionamento come se fosse sulla 1° bolla
041900050929     c                   if        §3afca = 2
042000050929     c                   if        §3atb2 = 'A3'
042100050929     c                   eval      gcpppc = 'D'
042200050929     c                   else
042300050929     c                   eval      §3afca = 1
042400050929     c                   end
042500050929     c                   end
042600050929     c* contrassegno sulla prima bolla. se Franco provv. mittente se
042700050929     c* assegnato prov. destinatario
042800050929     c                   if        §3afca = 1
042900050929     c                   if        %subst(§3ATB1: 1: 1)='F'
043000050929     c                   eval      gcpppc = 'M'
043100050929     c                   else
043200050929     c                   eval      gcpppc = 'D'
043300050929     c                   end
043400050929     c                   end
043500050929     c                   end
043600050929     c                   endsr
043700050921     C**------------------------------------------------------------
043800050921     C** data consegna richiesta
043900050921     C**------------------------------------------------------------
044000050921     C     srdcr         BEGSR
044100051011     C* tolgo un gg alla data immessa per data eseguibilità solo se scelto
044200051011     c* IN DATA altrimenti se scelto DOPO IL lascio la data immessa
044300051011     c                   move      d915ndcr      dataiso
044400051011     c                   if        %subst(d915costante: 1: 7)='IN DATA'
044500051011     c                   move      dataiso       data915           8 0
044600051125     c                   if        data915 > oggi
044700051011     c                   move      dataiso       dadata
044800051011     c                   move      dataiso       adata
044900051006     c                   CALL      'XSRLAV8'
045000051006     c                   PARM                    Wdata8
045100051006     c                   z-add     giolav        giolavs           1 0
045200051006    3c                   do        *hival
045300051006     c     dataiso       subdur    1:*d          dataiso
045400051006     c                   move      dataiso       dadata
045500051011     c                   move      data915       adata
045600051006     c                   CALL      'XSRLAV8'
045700051006     c                   PARM                    Wdata8
045800051006    4c                   if        giolav = 0 or giolav > giolavs
045900051006     c                   leave
046000051006    4c                   endif
046100051006    3c                   enddo
046200051125     c                   end
046300051014     c                   else
046400051014     c* per riconoscere se IN DATA metto 915 nel campo GNPDIF se DOPO IL
046500051014     c* metto 9152
046600051014     c                   z-add     9152          gnpdif
046700051011     c                   end
046800051006     c                   move      dataiso       gcpded
046900051107     c* se i gg lavorativi che intercorrono dalla data eseguibilità ( la
047000051107     c* data consegna richiesta) e la data in cui riceviamo le disposizioni
047100051107     c* sono maggiori di 3 rimetto la fase a 25
047200050921     c* in modo che la partenza la conferma
047300051107     c                   move      oggi          dadata
047400051011     c                   move      dataiso       adata
047500051006     c                   CALL      'XSRLAV8'
047600051006     c                   PARM                    Wdata8
047700050921    4c                   if        giolav > 3
047800050921     c                   eval      gcpfas = 25
047900050921    4c                   endif
048000050921     c*
048100050921     c                   endsr
048200050921     C**------------------------------------------------------------
048300050921     C** imposto il testo della nota
048400050921     C**------------------------------------------------------------
048500050921     C     srnota        BEGSR
048600050928     C                   CLEAR                   GCPDED
048700050921     c* note libere o nuovo indirizzo
048800050921     C                   MOVEL     VARDMC        GNPDMC
048900051013     c* memorizzo il progressivo nota nel numero fax in modo da poter
049000051013     c* riconoscere il tipo disposizione quando leggerò il file TIGNP00F
049100051013     c                   if        varnpr >= 900
049200051013     c                   z-add     varnpr        gnpdif
049300051013     c                   end
049400050921     c                   select
049500051013     c                   when      varnpr = 912
049600050921     C                   MOVEL     VARDMC        DGNPDMC912
049700051014     c                   eval      gnpdmc = d912costante + d912telefono
049800050921     c                   when      varnpr = 913
049900050921     C                   MOVEL     VARDMC        DGNPDMC913
050000051014     c                   eval      gnpdmc = d913costante + d913persona
050100050921     c                   when      varnpr = 915
050200050921     C                   MOVEL     VARDMC        DGNPDMC915
050300050921     c                   exsr      srdcr
050400050921     c                   movel     d915ndcr      com10            10
050500051014     c                   eval      gnpdmc = d915costante + com10
050600050921     c                   when      varnpr = 917
050700050921     C                   MOVEL     VARDMC        DGNPDMC917
050800051014     c                   eval      gnpdmc = d917costante + d917persona
050900050921     c                   when      varnpr = 924
051000050921     C                   MOVEL     VARDMC        DGNPDMC924
051100051014     c                   eval      gnpdmc = d924cap + d924loc +
051200051014     c                             d924pro +  d924naz
051300050921     c                   endsl
051400050921     c*
051500050921     c                   endsr
051600050921     C**------------------------------------------------------------
051700050921     C** aggancio tabelle neccessarie
051800050921     C**------------------------------------------------------------
051900050921     C     srtab         BEGSR
052000050921     C* TIPO BOLLA
052100050921     C                   MOVEL     '3A'          TBLCOD
052200050921     C                   MOVEL(p)  blpCBO        TBLKEY
052300050921     c                   clear                   ds3a
052400050921     C     KTAB          CHAIN     TABEL00f
052500050921     C                   if        %found(tabel00f)
052600050921     C                   MOVEL     TBLUNI        DS3A
052700050921     c                   end
052800050921     C* Carico particolarità
052900050921     C                   CLEAR                   DS7Q
053000050921     C     BLPGGA        IFNE      *BLANKS
053100050921     C                   MOVEL     '7Q'          TBLCOD
053200050921     C                   MOVEL(p)  blpgga        TBLKEY
053300050921     C     KTAB          CHAIN     TABEL00f
053400050921     C                   if        %found(tabel00f)
053500050921     C                   MOVEL     TBLUNI        DS7Q
053600050921     C                   END
053700050921     C                   END
053800050921     C* FIL. CHE HA APERTO LA GIACENZA
053900050921     C                   clear                   OG143
054000050921     C                   MOVEL     *blank        GIAEST            1
054100050921     C     gcpfgc        CHAIN     AZORG01L
054200050921     C                   if        %found(azorg01l) and orgfva <> '*'
054300050921     C                   MOVEL     ORGDE3        OG143
054400050921     C     §OgNTW        ifeq      'FED'
054500050921     C     §OgNTW        oreq      'EEX'
054600050921     C     §OgNTW        oreq      'EUP'
054700050921     C                   MOVEL     'E'           GIAEST
054800050921     c                   end
054900050921     C                   END
055000050921     c*
055100050921     c                   endsr
055200051108     C**************************************************************************
055300051108     C* esco con codice d'errore
055400051108     C**************************************************************************
055500051108     C     srfine        BEGSR
055600051108     c*
055700051108     c                   rolbk
055800051108     c* record allocato
055900051108     c                   if        %status = 1218
056000051108     c                   eval      kpjbu = '2'
056100051108     c                   else
056200051108     c                   eval      kpjbu = '1'
056300051108     c                   end
056400051108     c                   seton                                        lr
056500051108     c                   return
056600051108     c*
056700051108     c                   endsr
056800050928     C**------------------------------------------------------------
056900050928     C** OPERAZIONI INIZIALI
057000050928     C**------------------------------------------------------------
057100050928     C     *INZSR        BEGSR
057200050922     c*
057300050922     C     *ENTRY        PLIST
057400050922     C                   PARM                    KPJBA
057500050921     C*
057600050926     C                   Z-ADD     1             CODUT             1 0
057700050921     C                   Z-ADD     1             ACOKUT
057800050921     C                   Z-ADD     151           ACOKCC
057900980130     C* DEFINIZIONE CHIAVI
058000941014     C     KGIA          KLIST
058100941014     C                   KFLD                    VARAGC
058200941014     C                   KFLD                    VARFGC
058300941014     C                   KFLD                    VARNGC
058400990628     C                   KFLD                    FRG               2 0
058500941014     C                   Z-ADD     0             FRG
058600051021     C     KGIA1         KLIST
058700051021     C                   KFLD                    £VARAGC
058800051021     C                   KFLD                    £VARFGC
058900051021     C                   KFLD                    £VARNGC
059000051021     C                   KFLD                    FRG               2 0
059100941014     C*
059200941014     C     KNOT          KLIST
059300941014     C                   KFLD                    VARAGC
059400941014     C                   KFLD                    VARFGC
059500941014     C                   KFLD                    VARNGC
059600941014     C                   KFLD                    FRG
059700941014     C                   KFLD                    FAS               3 0
059800941014     C                   KFLD                    NPR               3 0
059900980130     C*
060000980130     C     KTAB          KLIST
060100980130     C                   KFLD                    CODUT
060200050921     C                   KFLD                    TBLCOD
060300050921     C                   KFLD                    TBLKEY
060400050927     C     KTAB1         KLIST
060500050927     C                   KFLD                    CODUT
060600050927     C                   KFLD                    TBLCOD
060700050921     C*
060800050921     C     KCLI          KLIST
060900050921     C                   KFLD                    ACOKUT
061000050921     C                   KFLD                    ACOKCC
061100050921     C                   KFLD                    ACOKSC
061200050921     C*
061300050929     C     Kspep         KLIST
061400050929     C                   KFLD                    gcpaas
061500050929     C                   KFLD                    gcplnp
061600050929     C                   KFLD                    gcpnrs
061700050929     C                   KFLD                    gcpnsp
061800980130     C*
061900050929     C     Kspea         KLIST
062000050929     C                   KFLD                    gcpaasa
062100050929     C                   KFLD                    gcplnpa
062200050929     C                   KFLD                    gcpnrsa
062300050929     C                   KFLD                    gcpnspa
062400050929     C*
062500941014     C                   Z-ADD     20            FAS
062600941014     C                   Z-ADD     999           NPR
062700941014     C*  Reperisco data del giorno
062800941014     C                   CLEAR                   WLBDA8
062900941014     C                   TIME                    WHHDAT           14 0
063000941014     C                   MOVE      WHHDAT        G02DAT
063100941014     C                   Z-ADD     0             G02INV
063200941014     C                   MOVEL     ' '           G02ERR
063300941014     C                   CALL      'XSRDA8'
063400941014     C                   PARM                    WLBDA8
063500941014     C                   Z-ADD     G02INV        OGGI              8 0
063600050926     C                   MOVEl     WHHDAT        ora               6 0
063700050112     C                   MOVEA     snot          NOTE
063800050927     c* carico tabella 2N x esito elaborazione
063900050927     c                   clear                   ds2n
064000050927     c                   clear                   xx
064100050927     c                   eval      tblcod = '2N'
064200050927     C     KTAB1         setll     TABEL00f
064300050927     c                   do        *hival
064400050927     C     KTAB1         reade     TABEL00f
064500050927     C                   if        %eof(tabel00f)
064600050927     c                   leave
064700050927     c                   end
064800050927     c                   if        tblflg <> ' '
064900050927     c                   iter
065000050927     c                   end
065100050927     C                   MOVEL     TBLUNI        DS2n
065200050927     c                   add       1             xx
065300050928     c                   movel     tblkey        esi(xx)
065400050927     c                   movel     §2Nvis        vis(xx)
065500050927     c                   enddo
065600941014     C*
065700941014     C                   ENDSR
065800050112**         snot
065900941014DATA DISPOS.CLIENTE:XXXXXXXX DATA AGGIOR.:XXXXXXXX
