000100980618      *-------------------------------------------------------------------------*
000200990618      *    INTERROGAZIONE BOLLE SEDE - PARZIALIZZAZIONI E SUBFILE               *
000300980618      *-------------------------------------------------------------------------*
000400980715
000500990618      ****************************************************************
000600990618      *  RIEPILOGO INDICATORI
000700990618      ****************************************************************
000800010112      * 15 - Inserita linea arrivo (selez.13)
000900990618      * 20 - GESTIONE SUBFILE
001000990618      * 21 - GESTIONE SUBFILE
001100990618      * 22 - GESTIONE SUBFILE
001200990618      * 23 - GESTIONE SUBFILE - ROLLUP
001300990707      * 24 - LETTURA FILE PER CARICAMENTO SUBFILE
001400990618      * 28 - ERRORE GENERICO DSPF
001500990618      * 30 - COMODO
001600010111      * 31 - COMODO
001700010112      * 32 - ok il record letto viene scritto nel subfile (selez.13)
001800000901      * 33 - COMODO
001900000714      * 35 - COMODO
002000000714      * 36 - COMODO
002100030529      * 37 - USATO PER PARZIALIZZAZIONE SI/NO/SOLO POSTE E X CTRL "VECCHIAIA" BOLLE PT
002200000717      * 38 - COMODO
002300990618      ***  Video 1
002400990618      * 40 - ERRORE linea di partenza spedizione
002500990618      * 41 - ERRORE anno spedizione
002600990618      * 42 - ERRORE riferimento mittente
002700990618      * 43 - ERRORE codice mittente
002800990621      * 44 - ERRORE riferimento partner
002900990621      * 45 - ERRORE codice cliente fatturazione
003000990621      * 46 - ERRORE data fatturazione
003100990621      * 47 - ERRORE data spedizione dal
003200990621      * 48 - ERRORE data spedizione al
003300990621      * 49 - ERRORE P.O. partenza
003400040216      * 50 - ERRORE mittente alfabetico
003500010112      * 51 - ERRORE segnacollo
003600000211      * 52 - ERRORE numero parcel DPD
003700000714      * 53 - ERRORE nr. serie per P.O. partenza
003800010219      * 54 - ERRORE n. orm
003900010219      * 55 - ERRORE p.o. emissione orm
004000040216      * 56 - ERRORE codice bolla
004100990623      ***  Video 2
004200990802      * 60 - POSIZIONAMENTO subfile
004300990618      ***
004400000714      * 90 - riemissione videata
004500030529      * 96 - COMODO
004600990618      ****************************************************************
004700990618
004800990618      ****************************************************************
004900990813      *  LEGENDA SELEZIONI
005000990618      ****************************************************************
005100990813      *  1 - SPEDIZIONE con chiave completa
005200040217      *  2 - RIFERIMENTO NUMERICO MITTENTE
005300040217      *  3 - RIFERIMENTO NUMERICO MITTENTE e CODICE MITTENTE
005400990618      *  4 - RIFERIMENTO PARTNER
005500990618      *  5 - CODICE FATTURAZIONE
005600990622      *  6 - CODICE FATTURAZIONE e (DATA FATTURA o SPED. NON FATTURATE)
005700990622      *  7 - CODICE FATTURAZIONE e DATA FATTURA e NR. FATTURA
005800010104      *  8 - DATA SPEDIZIONE e P.O. PARTENZA
005900000714      ** 9 - SPEDIZIONE con chiave parziale (***** ASTERISCATA *****)
006000000211      * 10 - NUMERO PARCEL DPD
006100000714      * 11 - DESTINATARIO e P.O. PARTENZA
006200000718      * 12 - P.O. PARTENZA e DATA e SERIE (SETGT)
006300010104      * 13 - PER SEGNACOLLO
006400000901      * 14 - CODICE "CHI SONO"
006500010219      * 15 - N.ORM
006600040217      * 16 - RIFERIMENTO ALFABETICO MITTENTE
006700990618      ****************************************************************
006800990618
006900990707     H DECEDIT('0,') DATEDIT(*DMY/)
007000990618
007100990618     fTNSB50D   CF   E             WORKSTN  sfile(SB50S02:nrr1)
007200990623     FTITAS30C  IF   E           K DISK
007300990623     FTITAS31C  IF   E           K DISK
007400990623     f                                     rename(TITAS000:TITAS031)
007500990623     f                                     rename(TITAS010:TITAS131)
007600990623     f                                     rename(TITASP00:TITASP31)
007700990623     FTITAS32C  IF   E           K DISK
007800990623     f                                     rename(TITAS000:TITAS032)
007900990623     f                                     rename(TITAS010:TITAS132)
008000990623     f                                     rename(TITASP00:TITASP32)
008100000717     FTITAS39C  IF   E           K DISK
008200000717     f                                     rename(TITAS000:TITAS039)
008300000717     f                                     rename(TITAS010:TITAS139)
008400000717     f                                     rename(TITASP00:TITASP39)
008500000717     FTITAS37C  IF   E           K DISK
008600000717     f                                     rename(TITAS000:TITAS037)
008700000717     f                                     rename(TITAS010:TITAS137)
008800000717     f                                     rename(TITASP00:TITASP37)
008900010104     FTITAS34C  IF   E           K DISK
009000010104     f                                     rename(TITAS000:TITAS034)
009100010104     f                                     rename(TITAS010:TITAS134)
009200010104     f                                     rename(TITASP00:TITASP34)
009300010112     FTITAS35C  IF   E           K DISK
009400010112     f                                     rename(TITAS000:TITAS035)
009500010112     f                                     rename(TITAS010:TITAS135)
009600010112     f                                     rename(TITASP00:TITASP35)
009700060608     FTITAS40C  IF   E           K DISK
009800060608     f                                     rename(TITAS000:TITAS040)
009900060608     f                                     rename(TITAS010:TITAS140)
010000060608     f                                     rename(TITASP00:TITASP40)
010100070927     F***TITA433C  IF   E           K DISK
010200070927     FTITA438C  IF   E           K DISK
010300060529     FTITA437C  IF   E           K DISK    rename(TITA4000:TITA4DPD)
010400000211     f                                     rename(TITA4P00:TITASPDPD)
010500010219     FTITA432C  IF   E           K DISK    rename(TITA4000:TITA4ORM)
010600010219     F                                     rename(TITA4P00:TITASPORM)
010700040216     FTITA435C  IF   E           K DISK    rename(TITA4000:TITA4rma)
010800040216     F                                     rename(TITA4P00:TITASPrma)
010900990622     FTITAA30C  IF   E           K DISK
011000010104     FTITAT24C  IF   E           K DISK
011100000901     FTITAH31C  IF   E           K DISK
011200110502     FTNBLA01L  IF   E           K DISK
011300990618     fAZORG01L  IF   E           K DISK
011400030529     FTNTBE01L  IF   E           K DISK
011500040216     fTABEL00F  if   e           k disk
011600990618      *------------------------------------------------------------------------*
011700990618     D PAXccc          s                   LIKE(ACOkcc)
011800990618     D PAXdmt          s                   LIKE(ACOrag)
011900060608     D savfiv          s                   LIKE(v1dfiv)
012000990618     D PAXdut          s             30
012100990618     D PAXdit          s              3
012200990618     D PAXflr          s             90
012300990618     D PAXsta          s              1  0
012400990618      *------------------------------------------------------------------------*
012500990623     D Kaas            S                   LIKE(TASaas)
012600990623     D Kmgs            S                   LIKE(TASmgs)
012700990629     D Ktrc            S                   LIKE(TAAtrc)
012800990623     D wlinea          S                   LIKE(TASlnp)
012900990623     D wanno           S                   LIKE(TASaas)
013000990623     D Wv1cccm         S                   LIKE(V1Cccm)
013100990623     D Wv1cdft         S                   LIKE(V1Cdft)
013200990623     D Wv1cdsa         S                   LIKE(V1Cdsa)
013300990623     D Wv1cdsd         S                   LIKE(V1Cdsd)
013400990623     D Wv1cksc         S                   LIKE(V1Cksc)
013500990623     D Wdatasp         S                   LIKE(V1Cdsd)
013600000718     D Wdatasp2        S                   LIKE(V1Cdsd)
013700990707     D WO69err         S                   LIKE(O69ERR)
013800000718     D Ksegna          S                   LIKE(TASNCD)
013900010112     D Klinea          S                   LIKE(TASlnp)
014000990618
014100120517     d $Full           s               n   inz
014200990802     D wselsped        S              1
014300990624     D wendpag         S              1
014400990618     D wtibs69r        s              1
014500990624     D W006A           S              6
014600070927     D*** W020A           S             20
014700070927     D W030A           S             30
014800060529     D W014A           S             14
014900990618     D xx              S              3  0
015000010129     D yy              S              3  0
015100990805     D WDATA_08G       S              8  0
015200000222     D W0030           S              3  0
015300000222     D W0030a          S              3  0
015400000222     D Wresto          S              3  0
015500990623     D W0040           S              4  0
015600990707     D W0060           S              6  0
015700990707     D W0080           S              8  0
015800000222     D W0140           S             14  0
015900000222     D W0150           S             15  0
016000990618     D Wdtgio          S              8  0
016100990618     D dateu           S              8  0
016200990623     D NUMspedcar      S              4  0
016300990618     D nrr1            S              4  0                                      Assoluto
016400990618     D nrr2            S              4  0                                      Comodo x lettura
016500990618     D nrr23           S              4  0                                      1° rcd pag x rollup
016600990802     D nrr60           S              4  0                                      Assoluto
016700990623     D Wspedpag        S              4  0
016800000211     D Wselez          S              2  0
016900010109     D xb              S              4  0
017000990618
017100990618     D DATA_eur        S               D   DATFMT(*eur)
017200061120     D wgiro           S              1
017300030529     D*
017400030529     D* VARIABILI DI WRK X CONSIDERAZIONI SU BOLLE PT
017500030529     D wrkoggiiso      s               d
017600030529     D wDataLimitePT   s              8  0
017700030529     D wGGLimPT        s              4  0
017800990618      *
017900990618      *   S C H I E R E
018000120517     D MSG             S             78    DIM(40) CTDATA PERRCD(1)             MSG VIDEO
018100120517     D NUM             S              1    DIM(10) CTDATA PERRCD(10)
018200000223     D SPED            S             16  0 DIM(90)
018300000223     D FILDPD          S              3s 0 DIM(20)
018400010129     D FILPOSTE        S              3s 0 DIM(20)
018500010109     D bolle           S             18    DIM(999)
018600060609     D w11             S              1    DIM(11)
018700990618      *
018800990618      *   D S   I N T E R N E / E S T E R N E
018900990630      *
019000000222     D                 DS
019100000222     D  §11                    1     11  0 dim(11)
019200000222     D  dpdchd                12     12  0
019300000222     D  dpdbrc                 1     12  0
019400000222      *
019500990805     D                 DS                  INZ
019600990805     D  WDATA_02               1      2  0
019700990805     D  WDATA_06               3      8  0
019800990805     D  WDATA_08               1      8  0
019900000223      *
020000000223     D                 DS                  INZ
020100000223     D  dsAAS                  1      4  0
020200000223     D  dsLNP                  5      7  0
020300000223     D  dsNRS                  8      9  0
020400000223     D  dsNSP                 10     16  0
020500000223     D  dsSPED                 1     16  0
020600010109
020700010111     D dsbolle         DS
020800010111     D  TASAAS
020900010111     D  TASLNP
021000010111     D  TASNRS
021100010111     D  TASNSP
021200010111     D  TASTBL
021300010219
021400010219     D ds_orm          DS                  inz
021500010219     D  v1cpoe                 1      3  0
021600010219     D  v1cnsr                 4      5  0
021700010219     D  v1cnor                 6     12  0
021800010219     D  v1cnrv                13     14  0
021900990618      *
022000041008      *?definizione oggetto reclamo come spedizione?
022100041008     d ds_OggSped      ds                  inz
022200041008     d  V2Clnp                             inz
022300041008     d  V2Cnrs                             inz
022400041008     d  V2Cnsp                             inz
022500041008     d  V2Haas                             inz
022600041008      *
022700990623     D KPJBA         E DS
022800041008     d FIDNA1ds      e ds
022900000717     D OG143         E DS
023000990726     D TNSB01DS      E DS                  INZ
023100990726     D TNSB50DS      E DS                  INZ
023200990624     D TNSB51DS      E DS                  INZ
023300990621     d TIBS69DS      E DS                  INZ
023400990618     d DS_cnaco      E DS                  extname(CNACO00F)
023500990618     d DS_cnind      E DS                  extname(CNIND00F)
023600990618     d DS_cnclp      E DS                  extname(CNCLP00F)
023700990618     d DS_fncls      E DS                  extname(FNCLS00F)
023800030529     D*
023900030529     D* DS X REPERIMENTO VARIABILI PGM OPERATIVI
024000030529     D DVPO          E DS
024100060608     D DSQC2         E DS
024200030529     D*------------
024300030529     D* DS REPERIMENTO DATI UTENTE
024400030529     D*------------
024500030529     D TIBS34DS      E DS                                                       *Profili utente
024600030529     D DDATIUTE      E DS                                                       *Dati utente
024700030529     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
024800990618      *
024900041008      *
025000041008     d Status         sds
025100041008     d  SDSprm           *parms
025200041008     D**************  SDS
025300990628     D  VTCPGM                 1     10
025400990727      *  titolo videata (lunghezza massima 32)
025500990727     D TIT_I           C                   CONST('* INTERROGAZIONE BOLLE DI SED-
025600990727     D                                     E *')
025700990727     D TIT_G           C                   CONST('*    GESTIONE BOLLE DI SEDE  -
025800990727     D                                       *')
025900060608     D FIV_Cost        C                   CONST('???=InizMese  ???=FineMese')
026000990618
026100990618      *------------------------------------------------------------------------*
026200990618      *---------------  C A L C O L O  ----------------------------------------*
026300990618      *------------------------------------------------------------------------*
026400990623
026500990623      * Controllo parametri ricevuti
026600990623     c                   clear                   TNSB50DS
026700990623     c                   SELECT
026800990623      *
026900990727     c                   WHEN       %SUBST(KPJBU:2:2) = '00'                    PArzializzazione
027000990623     c                   move      wdtgio        v1caas
027100990623     c                   movel     Kpjbu         I50OP0
027200990727     c                   movel     TIT_I         VTCtit
027300990623      *
027400990727     c                   WHEN       %SUBST(KPJBU:2:2) = '01'                    Parziali. con scelta
027500990623     c                   move      wdtgio        v1caas
027600990623     c                   movel     Kpjbu         I50OP0
027700990727     c                   movel     TIT_I         VTCtit
027800990705     c                   eval      v2dsel = '1'
027900990705      *
028000990705     c                   WHEN       %SUBST(KPJBU:2:2) = '02'                    Parzializzazione
028100990705     c                   move      wdtgio        v1caas                         per gestione
028200990705     c                   movel     Kpjbu         I50OP0
028300990727     c                   movel     TIT_G         VTCtit
028400990705     c                   eval      v2dsel = '2'
028500990623      *
028600990623     c                   WHEN       %SUBST(KPJBU:2:2) = '05'                    Visualizza spediz.
028700990623     c                   movel     Kpjbu         TNSB50DS
028800990623     c                   z-add     1             Wselez
028900990623     c                   z-add     D50aas        V1Caas
029000990623     c                   z-add     D50lnp        V1Clnp
029100990623     c                   z-add     D50nrs        V1Cnrs
029200990623     c                   z-add     D50nsp        V1Cnsp
029300990727     c                   movel     TIT_I         VTCtit
029400990623      *
029500990623     c                   OTHER                                                  opzione errata
029600990623     c                   eval      O50err = '1'
029700990623     c                   movel     MSG(1)        O50msg
029800990623     c                   movel     TNSB50DS      KPJBU
029900990623     c                   GOTO      FINE
030000990623     c                   ENDSL
030100000714
030200000714     C                   eval      v1csnp = 'N'
030300060608     c* Impostazione campi della prima videata
030400060608     c                   EXSR      IMPO
030500000714
030600990623      * Se richiesta la visualizzazione di una spedizione passo al caricamento del subfile
030700990624     c                   IF        %SUBST(I50OP0:2:2) <> '05'
030800990623      *
030900990618     C     for01         tag
031000990623      *
031100990618      * Emissione VIDEO1
031200990618     c                   write     SB50T01
031300990623     c                   exfmt     SB50D01
031400990623      *
031500990618      * Inizializzo Campi ed Indicatori
031600010219     c     40            do        55            XX
031700990618     c                   eval      *IN(XX) = *OFF
031800990618     c                   enddo
031900990618     c                   clear                   V1Cmsg
032000990618     c                   eval      *IN28 = *off
032100990618     c                   eval      *IN90 = *off
032200990623      *
032300990618      * f3=Fine
032400990618     c   kc              goto      fine
032500990623      *
032600990618      * Controlli 1a videata
032700990618     c                   exsr      ctrd01
032800990623      *
032900990622     c                   IF        *IN28 = *ON  or  *IN90 = *ON
033000990622     c                   goto      for01
033100990622     c                   ENDIF
033200990623      *
033300990623     c                   ENDIF
033400990623
033500990618      * Carico subfile
033600990618     C                   eval      *in23 = *off
033700990618     C                   clear                   nrr1
033800990623     c                   clear                   NUMspedcar
033900000223     c                   clear                   sped
034000061120     c                   clear                   wgiro
034100990618      *
034200990624     C                   exsr      RIEd02
034300990729      *
034400990729      * EMISSIONE SUBFILE O ENTRATA DIRETTA NELLA SPEDIZIONE
034500990624     c                   SELECT
034600990708      *
034700990729      * Trovati 1 + n record validi: emetto il subfile.
034800990706     C                   WHEN       NRR1 > 1
034900990729      *
035000990729      * Trovato 1 record valido. Interrogazione per selezione emetto il subfile
035100990729     C                   WHEN       NRR1 = 1  and  %SUBST(I50OP0:2:2) = '01'
035200990708      *
035300990729      * Trovato 1 record valido. Gestione della spedizione, se torna con errore emetto il subfile
035400990729     C                   WHEN       NRR1 = 1  and  %SUBST(I50OP0:2:2) = '02'
035500990729     c                   exsr      SUB_opz02
035600990729     c                   If        D01msg <> *blanks
035700990729     c                   movel     D01msg        V1Cmsg
035800990729     c                   eval      *in90 = *on
035900990730     c                   eval      *in28 = *on
036000990729     c                   Else
036100990729     c                   clear                   SB50D01
036200990729     c                   move      wdtgio        v1caas
036300000714     c                   eval      v1csnp = 'N'
036400990729     c                   clear                   WV1Cccm
036500990729     c                   clear                   WV1Cksc
036600060608     c                   eval      v1dfiv=savfiv
036700010112     c                   goto      FOR01
036800990729     c                   Endif
036900990729      * Trovato 1 record valido. Interrogazione, visualizzo la spedizione.
037000990729      *  Se chiamato con opz 05 torno al chiamante
037100990624     C                   WHEN       NRR1 = 1
037200990624     c                   exsr      SUB_opz05
037300990706     c                   clear                   SB50D01
037400990706     c                   move      wdtgio        v1caas
037500000714     c                   eval      v1csnp = 'N'
037600990707     c                   clear                   WV1Cccm
037700990707     c                   clear                   WV1Cksc
037800060608     c                   eval      v1dfiv=savfiv
037900990708     c                   If        %SUBST(I50OP0:2:2) = '05'
038000990708     c                   goto      FINE
038100990708     c                   Else
038200041008      * se richiamato da reclami e selezionato la bolla vado a fine
038300041008if  1c                   if            SDSprm > 1
038400041008     c                             and IA1ins = 'S'
038500041008     c                   goto      FINE
038600041008e   1c                   endif
038700041008      *
038800010112     c                   goto      FOR01
038900990708     c                   Endif
039000990708      *
039100990708      * Trovati 0 record validi e chiamato con opzione 05: ritorno al chiamante una segnalazione.
039200990729     c                   WHEN       NRR1 = *zeros and %SUBST(I50OP0:2:2) = '05'
039300990624     c                   eval      O50err = '1'
039400990726     c                   movel     msg(22)       O50msg
039500990624     c                   movel     TNSB50DS      KPJBU
039600990624     c                   goto      FINE
039700990708      *
039800990708      * Trovati 0 record validi e NON chiamato con opzione 05: emetto segnalazione e torno a VIDEO 1
039900990729     c                   WHEN       NRR1 = *zeros
040000990624     C     *INKL         DOWEQ     *OFF
040100990624     c                   write     SB50T01
040200990624     C                   exfmt     SB50D03
040300990624     C                   ENDDO
040400010112     c                   goto      FOR01
040500990624      *
040600990624     C                   ENDSL
040700990618
040800990618      * Emissione VIDEO2
040900990618     C     for02         tag
041000990618     C                   exsr      EMISd02
041100990618
041200990618      * F03 = Fine
041300990618     c   kc              goto      fine
041400990618
041500990708      * F12 = Precedente - Se chiamato con opzione "05" torno al chiamante
041600990707     c                   IF        *inKL = *ON
041700990708     c                   If        %SUBST(I50OP0:2:2) = '05'
041800990708     c                   goto      FINE
041900990708     c                   Else
042000990707     c                   clear                   SB50D01
042100990707     c                   move      wdtgio        v1caas
042200000714     c                   eval      v1csnp = 'N'
042300990707     c                   clear                   WV1Cccm
042400990707     c                   clear                   WV1Cksc
042500060608     c                   eval      v1dfiv=savfiv
042600010112     c                   goto      for01
042700990708     c                   Endif
042800990707     c                   ENDIF
042900990618
043000990618      * ROLLUP = Carico il subfile
043100990623     c                   IF        *IN23 = *ON
043200990618     c                   exsr      Ried02
043300990623     c                   If        Wspedpag = *zeros
043400990623     c                   movel     msg(21)       V1Cmsg
043500990618     c                   eval      *in28 = *on
043600990618     c                   Endif
043700990618     c                   GOTO      for02
043800990618     c                   ENDIF
043900990618
044000990618      * Controlli 2a videata
044100990618     c                   exsr      ctrd02
044200990618
044300990624      * Se selezionata una spedizione (Opz. 1) vado a fine altrimenti riemetto video 2
044400990624     c                   IF        Wselsped = '1'
044500990624     c                   movel     TNSB50DS      KPJBU
044600990624     c                   ELSE
044700990618     c                   goto      for02
044800990624     C                   ENDIF
044900990618
045000990618     c     fine          tag
045100990618
045200990618      * Chiudo pgm aperti
045300990618     C                   IF        Wtibs69r  <>  *BLANKS
045400990618     C                   eval      I69TLA  = 'C'
045500990618     C                   call      'TIBS69R'
045600990618     C                   parm                    TIBS69DS
045700990624     c                   ENDIF
045800990618
045900990624      * Imposto campi per uscita con F03
046000990624     c                   IF        *INKC = *ON
046100990624     c                   movel     TNSB50DS      W006A
046200990624     c                   clear                   TNSB50DS
046300990624     c                   movel     W006A         TNSB50DS
046400990624     c                   movel     '1'           O50F03
046500040311     c                   movel     TNSB50DS      KPJBU
046600990624     c                   ENDIF
046700990624
046800990618     c                   EVAL      *INLR = *ON
046900990618      **********************************************************************
047000060608      * Impostazione campi della prima videata
047100990618      **********************************************************************
047200060608     C     IMPO          BEGSR
047300060608     c* Chain tabella QC recod 2 per avere i libri iva
047400060608     c                   movel     CodUt         TBLkut
047500060608     c                   movel     'QC'          TBLcod
047600060608     c                   movel(p)  '2'           TBLkey
047700060608     c     K03TAB00      chain     TABEL
047800060608     c                   if        %found(tabel00f)
047900060608     c                   movel     tbluni        dsqc2
048000060608     c                   eval      v1dfiv=fiv_cost
048100060608     c                   eval      %subst(v1dfiv:1:3)=(%editc(§qcfiv: 'X'))
048200060608     c                   eval      %subst(v1dfiv:15:3)=(%editc(§qcfi2: 'X'))
048300060608     c                   endif
048400060608     c* salvo la stringa
048500060608     c                   movel     v1dfiv        savfiv
048600060608     c                   ENDSR
048700060608      **********************************************************************
048800060608      * CONTROLLI VIDEO 1
048900060608      **********************************************************************
049000060608     C     Ctrd01        BEGSR
049100990618
049200990621     C                   clear                   wselez
049300990621
049400990623      *  NUMERO SPEDIZIONE
049500990623     c                   IF        V1Cnsp <> *zeros  or  V1Clnp <> *zeros
049600990623      * Numero
049700990618     c                   IF        V1Cnsp = *zeros
049800990618     c                   eval        *IN40 = *on
049900990618     c                   eval        *IN28 = *on
050000990618     c                   eval        *IN90 = *on
050100990726     c                   movel     MSG(23)       V1Cmsg
050200990618     c                   ELSE
050300990623      * P.O
050400990623     C                   z-add     V1Clnp        Wlinea
050500990618     c                   exsr      CTR_linea
050600990618     c                   IF        *IN28 = *ON
050700990618     c                   eval        *IN40 = *on
050800990618     C                   ELSE
050900990623      * Anno
051000990623     c                   z-add     V1Caas        Wanno
051100990618     c                   exsr      CTR_anno
051200990618     c                   IF        *IN28 = *ON
051300990618     c                   eval        *IN41 = *on
051400990618     c                   else
051500990623     c                   z-add     Wanno         V1Caas
051600990618     c                   ENDIF
051700990618      *
051800990618     c                   ENDIF
051900990621     c                   ENDIF
052000990623     c                   ENDIF
052100990621      *
052200990621      * Controllo selezione
052300990621     c                   SELECT
052400990621     c                   WHEN      *IN28 = *ON
052500990621     c                   WHEN      V1Cnsp > *zeros
052600990621     c                   z-add     1             Wselez
052700990621     c                   ENDSL
052800990621      *
052900990621     c   28              goto      EndCTRD01
053000010219
053100010219      *  NUMERO ORM
053200010219     c                   IF        ds_orm <> *zeros
053300010219
053400010219      * obbligatorio il numero orm se inserito p.o.emissione
053500010219     C                   if        v1cpoe <> *zeros and v1cnor = *zeros
053600010219     c                   eval        *IN54 = *on
053700010219     c                   eval        *IN28 = *on
053800010219     c                   movel     MSG(31)       V1Cmsg
053900010219     C                   goto      endctrd01
054000010219     C                   endif
054100010219
054200010219      * obbligatorio il p.o. emissione se inserito il numero orm
054300010219     c                   if        v1Cpoe = *zeros  and  v1cnor <> *zeros
054400010219     c                   eval        *IN55 = *on
054500010219     c                   eval        *IN28 = *on
054600010219     c                   movel     MSG(32)       V1Cmsg
054700010219     C                   goto      endctrd01
054800010219     C                   endif
054900010219
055000010219      * già impostata una selezione
055100010219     c                   if        Wselez > *zeros
055200010219     c                   movel     MSG(4)        V1Cmsg
055300010219     c                   eval      *IN55 = *on
055400010219     c                   eval      *IN28 = *on
055500010219     C                   goto      endctrd01
055600010219     C                   endif
055700010219
055800010219      * non si può inserire la serie senza il p.o.e numero orm
055900010219     c                   if        V1Cnsr > *zeros  and
056000010219     c                             V1Cpoe = *zeros  and V1Cnor = *zeros
056100010219     c                   eval        *IN54 = *on
056200010219     c                   eval        *IN28 = *on
056300010219     c                   movel     MSG(34)       V1Cmsg
056400010219     C                   goto      endctrd01
056500010219     C                   endif
056600010219
056700010219      * non si può inserire il viaggio senza il p.o.e numero orm
056800010219     c                   if        V1Cnrv > *zeros  and
056900010219     c                             V1Cpoe = *zeros  and V1Cnor = *zeros
057000010219     c                   eval        *IN54 = *on
057100010219     c                   eval        *IN28 = *on
057200010219     c                   movel     MSG(34)       V1Cmsg
057300010219     C                   goto      endctrd01
057400010219     C                   endif
057500010219
057600010219      * Controlla il p.o.emissione
057700010219     C                   z-add     V1cpoe        Wlinea
057800010219     c                   exsr      CTR_linea
057900010219     c                   eval      *IN55 = (*in28 = *on)
058000010219     c   28              movel     MSG(33)       V1Cmsg
058100010219     C   28              goto      endctrd01
058200010219
058300010219      * imposta la selezione da fare
058400010219     C                   z-add     15            Wselez
058500010219     c                   ENDif
058600010112
058700010112      * SEGNACOLLO
058800010112     C                   if        v1cfls <> *zeros or v1cnsc <> *zeros
058900010112
059000010112      * obbligatorio il numero segnacollo se inserito p.o.segnacollo
059100010112     C                   if        v1cfls <> *zeros and v1cnsc =  *zeros
059200010112     c                   eval        *IN51 = *on
059300010112     c                   eval        *IN28 = *on
059400010112     c                   movel     MSG(17)       V1Cmsg
059500010112     C                   goto      endctrd01
059600010112     C                   endif
059700010112
059800010112      * obbligatorio il p.o. segnacollo se inserito il numero segnacollo
059900010112     c                   if        v1Cfls = *zeros  and  v1cnsc <> *zeros
060000010112     c                   eval        *IN51 = *on
060100010112     c                   eval        *IN28 = *on
060200010112     c                   movel     MSG(18)       V1Cmsg
060300010112     C                   goto      endctrd01
060400010112     C                   endif
060500010112
060600010112      * già impostata una selezione
060700010112     c                   if        Wselez > *zeros
060800010112     c                   movel     MSG(4)        V1Cmsg
060900010112     c                   eval      *IN51 = *on
061000010112     c                   eval      *IN28 = *on
061100010112     C                   goto      endctrd01
061200010112     C                   endif
061300010112
061400010112      * Controlla il p.o.segnacollo
061500010112     C                   z-add     V1cfls        Wlinea
061600010112     c                   exsr      CTR_linea
061700010112     c                   eval      *IN51 = (*in28 = *on)
061800010112     c   28              movel     MSG(7)        V1Cmsg
061900010112     C   28              goto      endctrd01
062000010112
062100010112      * imposta la selezione da fare
062200010112     C                   z-add     13            Wselez
062300010112     c                   ENDif
062400010112
062500010112      * non si può inserire la serie senza il p.o.e numero segnacollo
062600010112     c                   if        V1Cnrr > *zeros  and
062700010112     c                             V1Cfls = *zeros  and V1Cnsc = *zeros
062800010112     c                   eval        *IN51 = *on
062900010112     c                   eval        *IN28 = *on
063000010112     c                   movel     MSG(16)       V1Cmsg
063100010112     C                   goto      endctrd01
063200010112     C                   endif
063300990618
063400000901      * CODICE "CHI SONO"
063500000901     C     V1Cnot        IFNE      *blanks
063600000901     C     Wselez        IFEQ      *zeros
063700000901     C                   Z-ADD     14            Wselez
063800000901     C                   ELSE
063900000901     c                   movel     MSG(4)        V1Cmsg
064000000901     c                   eval      *IN40 = *on
064100000901     c                   eval      *IN28 = *on
064200000901     C                   ENDIF
064300000901     C                   ENDIF
064400000901      *
064500000901     c   28              goto      EndCTRD01
064600000901
064700040216      * RIFERIMENTO NUMERICO MITTENTE e CODICE CLIENTE MITTENTE
064800990618     c                   SELECT
064900990618      *    se immessa rag. sociale e non il codice mittente richiamo il pgm di ricerca alfabetica
065000990618     c                   WHEN      V1Cccm = *zeros  and  V1Dccm <> *blanks
065100990618     c                   movel(P)  V1Dccm        PAXdmt
065200990618     c                   exsr      ricalf
065300990618     C                   If        PAXsta > -1
065400000901     C                   movel     paxksm        V1Cccm
065500990621     c                   movel(P)  PAXdmt        V1Dccm
065600990618     C                   Endif
065700990618     c                   eval      *IN90 = *on
065800990621      *    se immesso il codice mittente lo decodifico
065900990618     c                   WHEN      V1Cccm > *zeros  and  V1Cccm <> WV1Cccm
066000990621     c                   clear                   V1Dccm
066100990621     c                   move      V1Cccm        w0040
066200990621     c                   IF        W0040 <> 9999  and  W0040 <> 8888
066300990621     c                   z-add     V1Cccm        I69kac
066400990621     c                   exsr      CTR_anagra
066500990707     C                   If         WO69ERR =  '1'
066600990623     c                   movel     MSG(2)        V1Cmsg
066700990621     c                   eval      *IN43 = *on
066800990621     c                   eval      *IN28 = *on
066900990621     c                   Else
067000990621     c                   eval      *IN90 = *on
067100990621     c                   movel     ACOrag        V1Dccm
067200990621     c                   Endif
067300990621     c                   ENDIF
067400990618      *    se inserito il codice mittente e non il riferimento mittente errore
067500990623     c                   WHEN      V1Cccm > *zeros  and  V1Crmn = *zeros
067600990623     c                   movel     MSG(3)        V1Cmsg
067700990621     c                   eval      *IN43 = *on
067800990621     c                   eval      *IN28 = *on
067900990621      *
068000990621     c                   ENDSL
068100990621      *
068200990621      * Controllo selezione
068300990621     c                   SELECT
068400990621     c                   WHEN      V1Crmn = *zeros
068500990621     c                   WHEN      *IN28 = *ON
068600990621     c                   WHEN      Wselez > *zeros
068700990623     c                   movel     MSG(4)        V1Cmsg
068800990621     c                   eval      *IN42 = *on
068900990621     c                   eval      *IN28 = *on
069000990621     c                   WHEN      V1Cccm > *zeros
069100990621     c                   z-add     3             Wselez
069200990621     c                   OTHER
069300990621     c                   z-add     2             Wselez
069400990621     c                   ENDSL
069500990621      *
069600990623     c                   z-add     V1Cccm        WV1Cccm
069700990623      *
069800990621     c   28              goto      EndCTRD01
069900040216
070000040216      * RIFERIMENTO ALFABETICO MITTENTE
070100040216      *   Controllo selezione
070200040216     c                   SELECT
070300040216     c                   WHEN      V1Crma = *blanks
070400040216     c                   WHEN      Wselez > *zeros
070500040216     c                   movel     MSG(4)        V1Cmsg
070600040216     c                   eval      *IN50 = *on
070700040216     c                   eval      *IN28 = *on
070800040216     c                   OTHER
070900040216     c                   z-add     16            Wselez
071000040216     c                   ENDSL
071100040216     c   28              goto      EndCTRD01
071200990618
071300990621      * RIFERIMENTO PARTNER
071400990621      *   Controllo selezione
071500990621     c                   SELECT
071600990623     c                   WHEN      V1Crpt = *blanks
071700990621     c                   WHEN      Wselez > *zeros
071800990623     c                   movel     MSG(4)        V1Cmsg
071900990621     c                   eval      *IN44 = *on
072000990621     c                   eval      *IN28 = *on
072100990621     c                   OTHER
072200990621     c                   z-add     4             Wselez
072300990621     c                   ENDSL
072400990621     c   28              goto      EndCTRD01
072500000211
072600000211      * NUMERO PARCEL DPD
072700060608     c* deve essere lungo o 11 o 14
072800060609     c                   select
072900060609     c* OK
073000060609     c                   when      v1cdpd=*blanks
073100060609     c                   when      %subst(v1cdpd:14:1)<>' '
073200060609     c* OK
073300060609     c                   when      %subst(v1cdpd:12:3)='   '  and
073400060609     c                             %subst(v1cdpd:11:1)<>' '
073500060609     c
073600060609     c* Se lungo 11 deve essere tutto numerico
073700060609     c                   movea     v1cdpd        w11
073800060609     c                   do        11            xx                3 0
073900060609     c     w11(XX)       lookup    num                                    30
074000060609     c                   if        not *in30
074100060609     c                   eval      *IN52 = *on
074200060609     c                   eval      *IN28 = *on
074300060609     c                   movel     MSG(39)       V1Cmsg
074400060609     c                   leave
074500060609     c                   endif
074600060609     c
074700060609     c                   enddo
074800060609     c
074900060609     c                   other
075000060609     c* parcel dpd errato
075100060609     c                   eval      *IN52 = *on
075200060609     c                   eval      *IN28 = *on
075300060609     c                   movel     MSG(39)       V1Cmsg
075400060609     c                   endsl
075500060609     c   28              goto      EndCTRD01
075600060609     c
075700000211      *   Controllo selezione
075800000211     c                   SELECT
075900060608     c                   WHEN      V1Cdpd = *blanks
076000000211     c                   WHEN      Wselez > *zeros
076100000211     c                   movel     MSG(4)        V1Cmsg
076200000211     c                   eval      *IN52 = *on
076300000211     c                   eval      *IN28 = *on
076400000211     c                   OTHER
076500000211     c                   z-add     10            Wselez
076600000211     c                   ENDSL
076700000211     c   28              goto      EndCTRD01
076800990621
076900990621      * CODICE CLIENTE FATTURAZIONE e DATA FATTURA e NUMERO FATTURA
077000060608     c*  P.O. IVA
077100990621      * Controllo data fattura
077200990621     C                   clear                   WV1Cdft
077300990621     C                   IF        V1Cdft  > *zeros
077400990805     C                   z-add     V1Cdft        WDATA_08
077500990805     C                   exsr      CTR_data
077600990805     C                   IF        *in28 = *on
077700990805     C                   movel     MSG(5)        V1CMSG
077800990805     c                   eval      *in46 = *ON
077900990805     C                   clear                   WV1Cdft
078000990805     C                   ELSE
078100990805     C                   z-add     Wdata_08g     WV1Cdft
078200990805     C                   z-add     Wdata_08      V1Cdft
078300990621     C                   ENDIF
078400990805     C                   ENDIF
078500990805      *
078600990805     c   28              goto      EndCTRD01
078700990621      *
078800990621     C                   SELECT
078900990621      *    se immessa rag. sociale e non il codice cliente richiamo il pgm di ricerca alfabetica
079000990621     c                   WHEN      V1Cksc = *zeros  and  V1Dksc <> *blanks
079100990621     c                   movel(P)  V1Dksc        PAXdmt
079200990621     c                   exsr      ricalf
079300990621     C                   If        PAXsta > -1
079400000901     C                   movel     paxksm        V1Cksc
079500990621     c                   movel(P)  PAXdmt        V1Dksc
079600990621     C                   Endif
079700990621     c                   eval      *IN90 = *on
079800990621      *    se immesso il codice cliente fattura lo decodifico
079900990621     c                   WHEN      V1Cksc > *zeros  and  V1Cksc <> WV1Cksc
080000990621     c                   clear                   V1Dksc
080100990621     c                   move      V1Cksc        w0040
080200990621     c                   IF        W0040 <> 9999  and  W0040 <> 8888
080300990621     c                   z-add     V1Cksc        I69kac
080400990621     c                   exsr      CTR_anagra
080500990707     C                   If         WO69ERR =  '1'
080600990623     c                   movel     MSG(2)        V1Cmsg
080700990621     c                   eval      *IN45 = *on
080800990621     c                   eval      *IN28 = *on
080900990621     c                   Else
081000990621     c                   eval      *IN90 = *on
081100990621     c                   movel     ACOrag        V1Dksc
081200990621     c                   Endif
081300990621     c                   ENDIF
081400060608      *    Se immesso codice cliente non immettere p.o. iva e viceversa     codice fatturazione
081500060608     c                   WHEN      V1Cfiv>0 and v1cksc>0
081600060608     c                   movel     MSG(37)       V1Cmsg
081700060608     c                   eval      *IN45 = *on
081800060608     c                   eval      *IN28 = *on
081900990623      *    non è possibile indicare la data fattura e richiedere le spedizioni non fatturate
082000060608     c                   WHEN      V1Cnfa <> *blanks  and
082100060608     c                             (v1cdft>0 or v1cfiv>0 or v1cnft>0)
082200990623     c                   movel     MSG(8)        V1Cmsg
082300990623     c                   eval      *IN46 = *on
082400990623     c                   eval      *IN28 = *on
082500060608      *    non è possibile indicare il numero fattura senza il codice cliente
082600060608      *    o P.O. iva
082700060608     c                   WHEN      V1Cksc = *zeros  and  V1Cnft > *zeros
082800060608     c                             and v1cfiv=0
082900990623     c                   movel     MSG(10)       V1Cmsg
083000990623     c                   eval      *IN46 = *on
083100990623     c                   eval      *IN28 = *on
083200060608      *    se immesso cliente e numero fattura immettere anche la data      e
083300060608     c                   WHEN      V1Cksc>0  and  V1Cnft > *zero
083400060608     c                             and v1cdft=0
083500060608     c                   movel     MSG(38)       V1Cmsg
083600060608     c                   eval      *IN46 = *on
083700060608     c                   eval      *IN28 = *on
083800990623      *    non è possibile indicare la data fattura senza il codice fatturazione
083900060608     c                   WHEN      v1Cksc = *zeros  and  V1Cdft > *zeros
084000060608     c                             and v1cnft=0 and v1cfiv=0
084100990623     c                   movel     MSG(11)       V1Cmsg
084200990621     c                   eval      *IN45 = *on
084300990621     c                   eval      *IN28 = *on
084400060608      *    Se immesso p.o. IVA immettere anche numero fattura               codice fatturazione
084500060608     c                   WHEN      V1Cfiv>0 and v1cnft=0
084600060608     c                   movel     MSG(36)       V1Cmsg
084700060608     c                   eval      *IN57 = *on
084800060608     c                   eval      *IN28 = *on
084900990623      *    non è possibile selezionare le spedizioni non fatturate senza il codice fatturazione
085000990623     c                   WHEN      V1Cksc = *zeros  and  V1Cnfa <> *blanks
085100990623     c                   movel     MSG(12)       V1Cmsg
085200990623     c                   eval      *IN45 = *on
085300990623     c                   eval      *IN28 = *on
085400990621      *
085500990621     c                   ENDSL
085600990621      *
085700990621      * Controllo selezione
085800990621     c                   SELECT
085900060608     c                   WHEN      V1Cksc = *zeros  and v1cfiv=0
086000990621     c                   WHEN      *IN28 = *ON
086100990621     c                   WHEN      Wselez > *zeros
086200990623     c                   movel     MSG(4)        V1Cmsg
086300990621     c                   eval      *IN45 = *on
086400990621     c                   eval      *IN28 = *on
086500060608     c                   WHEN      v1cfiv>0
086600060608     c                   z-add     17            Wselez
086700060608     c                   WHEN      v1cksc>0 and V1Cnft > *zeros
086800060608     c                   z-add     7             Wselez
086900060608     c                   WHEN      V1Cdft > *zeros  or  V1Cnfa <> *blanks
087000990621     c                   z-add     6             Wselez
087100990621     c                   OTHER
087200990621     c                   z-add     5             Wselez
087300990621     c                   ENDSL
087400990623      *
087500990623     c                   z-add     V1Cksc        WV1Cksc
087600990623      *
087700990621     c   28              goto      EndCTRD01
087800990618
087900990621      * DATA SPEDIZIONE DAL/AL
088000990621     C                   clear                   WV1Cdsd
088100990622     C                   clear                   WV1Cdsa
088200990623      *     spedizione al
088300990623     C                   IF        V1Cdsa  > *zeros
088400990805     C                   z-add     V1Cdsa        WDATA_08
088500990805     C                   exsr      CTR_data
088600990805     C                   IF        *in28 = *on
088700990805     c                   eval      *in48 = *ON
088800990805     C                   ELSE
088900990805     C                   z-add     Wdata_08g     WV1Cdsa
089000990805     C                   z-add     Wdata_08      V1Cdsa
089100990805     C                   ENDIF
089200990623     C                   ENDIF
089300990623      *     spedizione dal
089400110110     C                   IF        V1Cdsd  > *zeros and not *in28
089500990805     C                   z-add     V1Cdsd        WDATA_08
089600990805     C                   exsr      CTR_data
089700990805     C                   IF        *in28 = *on
089800990805     c                   eval      *IN47 = *on
089900990805     c                   eval      *IN48 = *off
090000990805     C                   ELSE
090100990805     C                   z-add     Wdata_08g     WV1Cdsd
090200990805     C                   z-add     Wdata_08      V1Cdsd
090300990805     C                   ENDIF
090400990621     C                   ENDIF
090500990622      *
090600990622      * Gestione errori, controllo congruità date e Controllo selezione
090700990621     c                   SELECT
090800990623     c                   WHEN      *IN47 = *ON  or  *IN48 = *ON
090900990623     c                   movel     MSG(13)       V1Cmsg
091000990623     c                   eval      *IN28 = *on
091100990622      *
091200990624     c                   WHEN      WV1Cdsa < WV1Cdsd  and  WV1Cdsa <> *zeros
091300990623     c                   movel     MSG(14)       V1Cmsg
091400990621     c                   eval      *IN28 = *on
091500990621     c                   eval      *IN47 = *on
091600010112      **!!!****
091700010112      *   obbligatoria la data se selezione 13 e segnacollo non univoco
091800010112     c                   WHEN      wselez = 13 and  V1flgs = *blanks
091900010112     C                             and v1cfls > *zeros and v1cdsd = *zeros
092000010112     c                   movel     MSG(27)       V1Cmsg
092100010112     c                   eval      *IN28 = *on
092200010112     c                   eval      *IN47 = *on
092300010112     c                   goto      EndCTRD01
092400010112      **!!!****
092500990621      *
092600990621     c                   WHEN      V1Cdsd = *zeros
092700000714      *
092800000714      *              obbligatorio P.O. partenza
092900000714     c                   WHEN      wselez = *zeros  and  V1Clp1 = *zeros
093000000714     c                   movel     MSG(25)       V1Cmsg
093100000714     c                   eval      *IN28 = *on
093200000714     c                   eval      *IN49 = *on
093300010112
093400000714      *              se selezionato il codice fatturazione (sel. 5) vince data spedizione
093500000714      ******** ! >>> se si disasterisca ATTENZIONE xchè è stata aggiunta l'obbligatorietà
093600000714      ******** ! >>> del P.O. di partenza ed è stata cambiata la routine per selezione = 8
093700000714     c******** ! >>>     WHEN      Wselez = 5
093800000714     c******** ! >>>     z-add     8             Wselez
093900000714      *
094000991103     c                   WHEN      Wselez = *zeros
094100991103     c                   z-add     8             Wselez
094200991103      *
094300990813      *
094400990621     c                   ENDSL
094500990622      *
094600990622     C                   If        WV1Cdsa = *zeros
094700990622     c                   z-add     WV1Cdsd       WV1Cdsa
094800990622     C                   Endif
094900990621     c   28              goto      EndCTRD01
095000000718      *
095100000718      * max mese successivo
095200000718     C     Wv1cdsd       ifgt      0
095300000718     C                   movel     Wv1cdsd       WorkAnno          4 0
095400000718     C                   move      Wv1cdsd       Workmesegg        4 0
095500000718     C                   move      '31'          Workmesegg
095600000718     C     Workmesegg    ifeq      1231
095700000718     C                   z-add     131           Workmesegg
095800000718     C                   add       1             Workanno
095900000718     C                   else
096000000718     C                   add       100           Workmesegg
096100000718     C                   endif
096200000718     C                   movel     workanno      workdata          8 0
096300000718     C                   move      workmesegg    workdata
096400000718     C     wv1cdsa       ifgt      workdata
096500000718     c                   movel     MSG(29)       V1Cmsg
096600000718     C                   seton                                        28  48
096700000718     C                   endif
096800000718     C                   endif
096900000718     c   28              goto      EndCTRD01
097000040216
097100040216      * CODICE BOLLA
097200040216if  1c                   if        V1Ccbo <> *blanks
097300040216     c                   movel     CodUt         TBLkut
097400040216     c                   movel     '3A'          TBLcod
097500040216      *
097600040216     c                   eval      *in30  =  *off
097700040216     c     '?'           scan      V1Ccbo                                 30
097800040216if  2c                   if        *in30 = *on
097900040216     c                   call      'X§TABER'
098000040216     c                   parm                    TBLkut
098100040216     c                   parm                    TBLcod
098200040216     c                   parm                    TBLkey
098300040216     c                   parm                    §DES             30
098400040216     c                   movel     TBLkey        V1Ccbo
098500040216     c                   eval      *in90 = *on
098600040216e   2c                   endif
098700040216      *
098800040216     c                   movel(p)  V1Ccbo        TBLkey
098900040216     c     K03TAB00      chain     TABEL
099000040216if  2c                   if        NOT %found(TABEL00F)
099100040216     c                             or  TBLflg <> *blanks
099200040216     c                   movel     Msg(35)       V1Cmsg
099300040216     c                   seton                                        285690
099400040216     c                   goto      EndCtrD01
099500040216e   2c                   endif
099600040216e   1c                   endif
099700990621
099800000714      * DESTINATARIO
099900000714     C     Wselez        ifeq      *zeros
100000000714     C     V1Crsd        andne     *blanks
100100000714     C                   z-add     11            Wselez
100200000714     C     V1Clp1        ifeq      *zeros
100300000714     c                   eval      *IN28 = *on
100400000714     c                   eval      *IN49 = *on
100500000714     c                   movel     MSG(25)       V1Cmsg
100600000714     C                   endif
100700000714     C                   endif
100800000714     c   28              goto      EndCTRD01
100900000714
101000000714      * P.O. PARTENZA
101100990621     c                   clear                   V1Dlp1
101200990621     C                   IF        V1Clp1 > *zeros
101300990623     C                   z-add     V1Clp1        Wlinea
101400990621     c                   exsr      CTR_linea
101500990621     c                   If        *IN28 = *ON
101600990621     c                   eval        *IN49 = *on
101700990623     c                   movel     MSG(7)        V1Cmsg
101800990621     c                   Else
101900990621     C                   movel     ORGDES        V1Dlp1
102000000717     C     Wselez        ifeq      0
102100000717     c                   movel     MSG(27)       V1Cmsg
102200000718     C                   seton                                        47  28
102300000717     C                   endif
102400990621     C                   Endif
102500990621     C                   ENDIF
102600990621     c   28              goto      EndCTRD01
102700000714
102800000714      * SERIE
102900000714     C                   setoff                                       35  36
103000000714     C     v1cser        ifne      '  '
103100010129     C                   if        v1cnrs <> *zeros
103200010129     C                             or v1cnrr <> *zeros
103300010129     C                   movel     MSG(26)       V1Cmsg
103400010129     C                   eval      *IN53 = *on
103500010129     C                   eval      *IN28 = *on
103600010129     C                   goto      EndCTRD01
103700010129     C                   endif
103800000714     C                   movel     v1cser        wserie1           1
103900000714     C                   move      v1cser        wserie2           1
104000000714     C     wserie2       ifeq      ' '
104100000714     C                   eval      wserie2 = wserie1
104200000714     C                   eval      wserie1 = ' '
104300000714     C                   movel     wserie1       v1cser
104400000714     C                   move      wserie2       v1cser
104500000714     C                   endif
104600000714     C     wserie1       ifeq      ' '
104700000714     C                   eval      wserie1 = '0'
104800000714     C                   endif
104900000714      *
105000000714     C     wserie1       ifeq      '0'
105100000714     C                   seton                                        35
105200000714     C                   endif
105300000714     C  N351             do        9             sr                2 0
105400000714     C                   move      sr            wnumalfa          1
105500000714     C     wserie1       ifeq      wnumalfa
105600000714     C                   seton                                        35
105700000714     C                   endif
105800000714     C                   enddo
105900000714      *
106000000714     C     wserie2       ifeq      '0'
106100000714     C                   seton                                        36
106200000714     C                   endif
106300000714     C  N361             do        9             sr
106400000714     C                   move      sr            wnumalfa          1
106500000714     C     wserie2       ifeq      wnumalfa
106600000714     C                   seton                                        36
106700000714     C                   endif
106800000714     C                   enddo
106900000714      *
107000000714     C     *in35         ifeq      *off
107100000714     C     *in36         oreq      *off
107200000714     c                   movel     MSG(24)       V1Cmsg
107300000714     c                   eval      *IN53 = *on
107400000714     c                   eval      *IN28 = *on
107500000717     C                   else
107600000717     C     Wselez        ifeq      8
107700000717     C                   z-add     12            Wselez
107800000717     C                   endif
107900000714     C                   endif
108000000714     C                   endif
108100000714     c   28              goto      EndCTRD01
108200990621
108300990621      * P.O. ARRIVO
108400990621     c                   clear                   V1Dlna
108500990621     C                   IF        V1Clna > *zeros
108600990623     C                   z-add     V1Clna        Wlinea
108700990621     c                   exsr      CTR_linea
108800990621     c                   If        *IN28 = *ON
108900010104     c                   eval        *IN54 = *on
109000990623     c                   movel     MSG(7)        V1Cmsg
109100990621     c                   Else
109200990621     C                   movel     ORGDES        V1Dlna
109300990621     C                   Endif
109400990621     C                   ENDIF
109500990621     c   28              goto      EndCTRD01
109600990813
109700990813      * CONTROLLO SELEZIONE VALIDA
109800990813     c                   IF        Wselez = *zeros
109900990813     c                   movel     MSG(15)       V1Cmsg
110000990813     c                   eval      *IN40 = *on
110100990813     c                   eval      *IN28 = *on
110200990813     c                   ENDIF
110300990813     c   28              goto      EndCTRD01
110400990621
110500990618     c     Endctrd01     ENDSR
110600990618      *****************************************************************
110700990618      * CARICAMENTO SUBFILE VIDEO 2
110800990618      *****************************************************************
110900990618     C     RIED02        BEGSR
111000120517      *
111100120517      * - Già carico il n° max di rec. nel sfl !!!
111200120517     c                   if        $Full
111300120517     c                   eval      Wendpag = 'S'
111400120517     c                   eval      V1Cmsg  = Msg(40)
111500120517     c                   eval      *in28 = *on
111600120517     c                   leavesr
111700120517     c                   endif
111800990618      *
111900990618      * Pulisco subfile
112000990618     c                   IF        *IN23 = *off
112100990618     C                   eval         *IN21 = *OFF
112200990622     C                   WRITE     SB50C02
112300990618     C                   eval         *IN20 = *ON
112400990618     C                   eval         *IN21 = *ON
112500990618     c                   ENDIF
112600990618      *
112700990618     c                   clear                   Wendpag
112800990623     c                   clear                   Wspedpag
112900990623     c                   z-add     NUMspedcar    NRR1
113000990622      *
113100990618     c                   SELECT
113200990622      * spedizione
113300990622     C                   WHEN      Wselez = 1
113400990618     C                   exsr      RIESEL1
113500040216      * riferimento mittente numerico
113600990622     C                   WHEN      Wselez = 2
113700990622     C                   exsr      RIESEL2
113800040216      * riferimento mittente numerico e codice mittente
113900990622     C                   WHEN      Wselez = 3
114000990622     C                   exsr      RIESEL3
114100990622      * riferimento partner
114200990622     C                   WHEN      Wselez = 4
114300990622     C                   exsr      RIESEL4
114400990622      * codice cliente fatturazione e (data fattura o non fatturate)
114500990707     C                   WHEN      Wselez = 5  or  Wselez = 6
114600990707     C                   exsr      RIESEL56
114700990622      * codice cliente fatturazione e data fattura e numero fattura
114800990622     C                   WHEN      Wselez = 7
114900990622     C                   exsr      RIESEL7
115000000717      * data spedizione (e lnp)
115100990622     C                   WHEN      Wselez = 8
115200990622     C                   exsr      RIESEL8
115300990813      * lnp e data spedizione
115400991103     C*                  WHEN      Wselez = 9
115500991103     C*                  exsr      RIESEL9
115600000211      * numero parcel DPD
115700000211     C                   WHEN      Wselez = 10
115800000222     C                   exsr      M1031
115900000222     C                   exsr      RIESEL10
116000000717      * lnp e destinatario
116100000717     C                   WHEN      Wselez = 11
116200000717     C                   exsr      RIESEL11
116300000717      * anno + lnp + serie
116400000717     C                   WHEN      Wselez = 12
116500000717     C                   exsr      RIESEL12
116600000717      * secco per segnacollo su TITAT27C
116700000717     C                   WHEN      Wselez = 13
116800000717     C                   exsr      RIESEL13
116900000901      * codice "CHI SONO"
117000000901     C                   WHEN      Wselez = 14
117100000901     C                   exsr      RIESEL14
117200010219      * numero ORM
117300010219     C                   WHEN      Wselez = 15
117400010219     C                   exsr      RIESEL15
117500040216      * riferimento mittente alfabetico
117600040216     C                   WHEN      Wselez = 16
117700040216     C                   exsr      RIESEL16
117800060608     c
117900060608     c* P.O. iva e numero fattura
118000060608     C                   WHEN      Wselez = 17
118100060608     C                   exsr      RIESEL17
118200990618      *
118300990618     C                   ENDSL
118400990618      *
118500990618     c                   z-add     1             V2Cnrr
118600990623     c                   z-add     NRR1          NUMspedcar
118700990618      *
118800990618     c                   ENDSR
118900990707      *****************************************************************
119000990707      * CARICAMENTO SUBFILE * SELEZIONE 1 - spedizione
119100990707      *****************************************************************
119200990707     C     RIESEL1       BEGSR
119300990707      *
119400990707      * Se il chiamante ha passato la key sped. con il tipo bolla chaino con chiave completa
119500990707     c                   IF        D50tbl <> *blanks
119600990707     c                             and  %SUBST(I50OP0:2:2) <> '05'
119700990707     C     kspe1         chain     TITAS30C                           24
119800990707     C  N24              exsr      RICsbfl
119900990707     C                   ELSE
120000990707      *
120100990707     c                   IF        *IN23 = *OFF
120200990707     C     kspe          chain     TITAS30C                           24
120300990707     c                   ENDIF
120400990707      *
120500990707     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
120600030529     C*
120700030529     C* Inserisco qui i controlli x verificare la data limite x considerare le
120800030529     C* bolle PT (tale controllo nn viene effettuato se l'utente è del gruppo
120900030529     C* EDP o ISP).
121000030529     C                   if        %subst(KPJBU:2:2) <> '05'
121100030529     C                   if        %subst(KNMUS:1:3) = 'EDP'
121200030529     C                   else
121300030529     C     tasLNP        lookup    FILPOSTE                               96
121400030529     C                   if        *in96
121500030529     C                   if        tasDCM >  0
121600030529     C                   if        tasDCM >= wDataLimitePT
121700030529     C                   else
121800030529     C                   seton                                        24
121900030529     C                   endif
122000030529     C                   endif
122100030529     C                   endif
122200030529     C                   endif
122300030529     C                   endif
122400030529     C*
122500030529     C                   if        *in24 = *off
122600990707     C                   exsr      RICsbfl
122700990707     C     kspe          reade     TITAS30C                               24
122800030529     C                   endif
122900990707     C                   ENDDO
123000990707      *
123100990707     c                   ENDIF
123200990707      *
123300990707      *  Se non ho trovato record aggancio il file delle bolle annullate
123400990707     c                   IF        NUMspedcar = *zeros
123500110502     C     kspe          chain     TNBLA01L                           24
123600110502     c                   If        *in24 = *off
123700110502     c                   movel     msg(19)       V1Cmsg
123800110502     c                   Endif
123900990707     c                   ENDIF
124000990707      *
124100990707     c                   ENDSR
124200990707      *****************************************************************
124300990707      * CARICAMENTO SUBFILE * SELEZIONE 2 - Rif. mittente
124400990707      *****************************************************************
124500990707     C     RIESEL2       BEGSR
124600990707      *
124700990707     c                   IF        *IN23 = *OFF
124800990707     C     V1Crmn        chain     TITAS32C                           24
124900990707     c                   ENDIF
125000990707      *
125100000211     c                   DOW       *in24=*Off  and  Wendpag <> 'S'
125200990707     C                   exsr      CTR_selez
125300990707     C     V1Crmn        reade     TITAS32C                               24
125400990707     C                   ENDDO
125500990707      *
125600990707     c                   ENDSR
125700990707      *****************************************************************
125800990707      * CARICAMENTO SUBFILE * SELEZIONE 3 - Rif. mittente + codice
125900990707      *****************************************************************
126000990707     C     RIESEL3       BEGSR
126100990707      *
126200990707     c                   IF        *IN23 = *OFF
126300990707     C     Krmn          chain     TITAS32C                           24
126400990707     c                   ENDIF
126500990707      *
126600990707     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
126700990707     C                   exsr      CTR_selez
126800990707     C     Krmn          reade     TITAS32C                               24
126900990707     C                   ENDDO
127000990707      *
127100990707     c                   ENDSR
127200990707      *****************************************************************
127300990707      * CARICAMENTO SUBFILE * SELEZIONE 4 - Rif. partner
127400990707      *****************************************************************
127500990707     C     RIESEL4       BEGSR
127600990707      *
127700070927     c***                movel(P)  V1Crpt        W020A
127800070927     c                   movel(P)  V1Crpt        W030A
127900990707      *
128000990707     c                   IF        *IN23 = *OFF
128100070927     C     W030A         chain     TITA438C                           33
128200000901     C  N33Ktas          chain     TITAS30C                           24
128300990707     c                   ENDIF
128400990707      *
128500000901     c                   DOW       *IN33 = *off
128600000901     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
128700990707     C                   exsr      CTR_selez
128800040217     c     Wendpag       cabeq     'S'           EndSel04
128900000901     C     Ktas          reade     TITAS30C                               24
129000990707     C                   ENDDO
129100040217     c     Wendpag       cabeq     'S'           EndSel04
129200070927     C     W030A         reade     TITA438C                               33
129300000901     C  N33Ktas          chain     TITAS30C                           24
129400000901     C                   ENDDO
129500990707      *
129600040217     c     EndSel04      ENDSR
129700990622      *****************************************************************
129800990707      * CARICAMENTO SUBFILE * SELEZIONE 5/6 - Cliente fatturazione e data fattura/non fatturate
129900990622      *****************************************************************
130000990707     C     RIESEL56      BEGSR
130100990622      *
130200990622     c                   IF        *IN23 = *OFF
130300990707     C                   IF        Wselez = 5
130400990707     C     Kfat1         setll     TITAS31C
130500990707     c                   ELSE
130600990707     C     kfat2         setll     TITAS31C
130700990622     c                   ENDIF
130800990825     c
130900990825     C                   IF        Wselez = 5
131000990707     C     KFAT1         reade     TITAS31C                               24
131100990825     c                   else
131200990825     C     kfat2         reade     TITAS31C                               24
131300990825     c                   endif
131400990707     c                   ENDIF
131500990622      *
131600990707     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
131700990622     C                   exsr      CTR_selez
131800990825     C                   IF        Wselez = 5
131900990707     C     KFAT1         reade     TITAS31C                               24
132000990825     c                   else
132100990825     C     kfat2         reade     TITAS31C                               24
132200990825     c                   endif
132300990622     C                   ENDDO
132400990622      *
132500990622     c                   ENDSR
132600990707      *****************************************************************
132700990707      * CARICAMENTO SUBFILE * SELEZIONE 7 - Codice cliente fatturazione e data fattura e n. fattura
132800990707      *****************************************************************
132900990707     C     RIESEL7       BEGSR
133000990707      *
133100990707     c                   IF        *IN23 = *OFF
133200990716     C     KFAT3         chain     TITAS31C                           24
133300990707     c                   ENDIF
133400990707      *
133500990707     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
133600990707     C                   exsr      CTR_selez
133700990716     C     KFAT3         reade     TITAS31C                               24
133800990707     C                   ENDDO
133900990707      *
134000990707     c                   ENDSR
134100060608      *****************************************************************
134200060608      * CARICAMENTO SUBFILE * SELEZIONE 17 - P.O, iva e numero fattura      ta fattura e n. fattura
134300060608      *****************************************************************
134400060608     C     RIESEL17      BEGSR
134500060608      *
134600060608     c                   IF        *IN23 = *OFF
134700060608     c                   if        v1cdft=0
134800060608     C     KFAT4         chain     TITAS40C                           24
134900060608     c                   else
135000060608     C     KFAT43        chain     TITAS40C                           24
135100060608     c                   endif
135200060608     c
135300060608     c                   ENDIF
135400060608      *
135500060608     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
135600060608     C                   exsr      CTR_selez
135700060608     c                   if        v1cdft=0
135800060608     C     KFAT4         reade     TITAS40C                               24
135900060608     c                   else
136000060608     C     KFAT43        reade     TITAS40C                               24
136100060608     c                   endif
136200060608     C                   ENDDO
136300060608      *
136400060608     c                   ENDSR
136500990707      *****************************************************************
136600990707      * CARICAMENTO SUBFILE * SELEZIONE 8 - data spedizione
136700990707      *****************************************************************
136800990707     C     RIESEL8       BEGSR
136900000718      *
137000990707     c                   IF        *IN23 = *OFF
137100990707     c                   movel     WV1Cdsd       Kaas
137200990707     c                   move      WV1Cdsd       Kmgs
137300010112     C                   eval      klinea = v1clp1
137400990707      *
137500000717     C     kdta          setll     TITAS39C
137600000717     C     v1cLP1        reade     TITAS39C                               24
137700990707     c                   ENDIF
137800990707      *
137900990707     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
138000990707     C                   exsr      CTR_selez
138100990707      * Se supero data limite fine lettura
138200990707     C                   If        Wdatasp > WV1Cdsa
138300990707     C                   eval      *IN24 = *on
138400990707     c                   Else
138500000717     C     v1cLP1        reade     TITAS39C                               24
138600990707     c                   Endif
138700990707     C                   ENDDO
138800000718      *
138900000718     c     endsbr8       ENDSR
139000990813      *****************************************************************
139100990813      * CARICAMENTO SUBFILE * SELEZIONE 9 - LNP e data spedizione
139200990813      *****************************************************************
139300991103     C*    RIESEL9       BEGSR
139400990813      *
139500991103     c*                  IF        *IN23 = *OFF
139600991103     C*    kspe9         setll     TITAS30C
139700991103     C*    Kspe9         reade     TITAS30C                               24
139800991103     c*                  ENDIF
139900990813      *
140000991103     c*                  DOW       *IN24 = *off  and  Wendpag <> 'S'
140100991103     C*                  exsr      CTR_selez
140200990813      * Se supero data limite fine lettura
140300991103     C*                  If        Wdatasp > WV1Cdsa
140400991103     C*                  eval      *IN24 = *on
140500991103     c*                  Else
140600991103     C*    Kspe9         reade     TITAS30C                               24
140700991103     c*                  Endif
140800991103     C*                  ENDDO
140900990813      *
141000991103     c*                  ENDSR
141100000222      * ------------------------------------------------------------------*
141200000222      *  CALCOLA CHECK DIGIT PER RIFERIMENTO DPD
141300000222      * ------------------------------------------------------------------*
141400000222     C     M1031         BEGSR
141500060607     c                   clear                   w0150
141600060607     c
141700060607     c                   move      v1cdpd        w003a             3
141800060607     c                   if        w003a=*blanks
141900000222      *
142000060609     C                   MOVEL(P)  v1cdpd        DPDBRC
142100000222      *
142200000222     c                   clear                   W0030a
142300000222     c                   clear                   w0030
142400000222      *
142500000222      * TOTALIZZO I VALORI, QUELLI NELLE POSIZIONI DISPARI PER 3
142600000222     c                   DO        11            xx
142700000222      *
142800000222     c     xx            div       2             W0030a
142900000222     c                   mvr                     Wresto
143000000222     c                   If        Wresto > *zeros
143100000222     c                   eval       W0030 = W0030 + (§11(xx) * 3)
143200000222     c                   Else
143300000222     c                   add       §11(xx)       W0030
143400000222     c                   Endif
143500000222      *
143600000222     c                   ENDDO
143700000222      *
143800000222     c     w0030         add       9             W0030a
143900000222     c                   move      0             W0030a
144000000222     c     W0030a        sub       w0030         dpdchd
144100000222     c
144200000222     C                   Z-ADD     DPDBRC        W0150
144300060607     c                   endif
144400000222      *
144500000222     c                   endsr
144600000211      *****************************************************************
144700000211      * CARICAMENTO SUBFILE * SELEZIONE 10 - Numero Parcel DPD
144800000211      *****************************************************************
144900000211     C     RIESEL10      BEGSR
145000000211      *
145100060607      * Ricerco sia import che export Export in TITA4
145200061120     c                   if        wgiro<>'2'
145300060529     c                   movel(p)  V1Cdpd        W014A
145400061120     c                   endif
145500000211      *
145600000211     c                   IF        *IN23 = *OFF
145700060529     C     W014A         chain     TITA437C                           33
145800000901     C  N33Ktas          chain     TITAS30C                           24
145900000211     c                   ENDIF
146000000211      *
146100061120     c     rsel10_tag    tag
146200061120     c*
146300000901     c                   DOW       *IN33 = *off
146400000901     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
146500000223     C                   exsr      ctrspeddpd
146600000223     C  N31              exsr      CTR_selez
146700040217     c     Wendpag       cabeq     'S'           EndSel10
146800000901     C     Ktas          reade     TITAS30C                               24
146900000901     C                   ENDDO
147000040217     c     Wendpag       cabeq     'S'           EndSel10
147100060529     C     W014A         reade     TITA437C                               33
147200000901     C  N33Ktas          chain     TITAS30C                           24
147300000901     C                   ENDDO
147400000211      *
147500060607      * Ricerco Import nel Rif mittente numerico  solo se immesso
147600060607      *  un numero di 11 cifre (visto che il new sarà alfa e lungo 14)
147700061120     c                   if        w0150>0 and wgiro<>'2'
147800060607     c                   IF        *IN23 = *OFF
147900060607     C     W0150         chain     TITAS32C                           24
148000060607     c                   ENDIF
148100000211      *
148200060607     c                   DOW       *in24=*Off  and  Wendpag <> 'S'
148300060607     C                   exsr      ctrspeddpd
148400060607     C  N31              exsr      CTR_selez
148500060607     C     W0150         reade     TITAS32C                               24
148600060607     C                   ENDDO
148700061120     c*
148800061120     c                   eval      w014a='0'+ %subst(v1cdpd:1:3)+'99'+
148900061120     c                             %subst(v1cdpd:4:8)
149000061120     C     W014A         chain     TITA437C                           33
149100061120     C  N33Ktas          chain     TITAS30C                           24
149200061120     c                   eval      wgiro='2'
149300061120     c                   goto      rsel10_tag
149400060607     c
149500060607     c                   ENDIF
149600000211      *
149700040217     c     EndSel10      ENDSR
149800000717      *****************************************************************
149900000717      * CARICAMENTO SUBFILE * SELEZIONE 11 - lnp + destinatario
150000000717      *****************************************************************
150100000717     C     RIESEL11      BEGSR
150200000717      *
150300000717     c                   IF        *IN23 = *OFF
150400000717      *
150500000717     C     kspe11        setll     TITAS37C
150600000717     C     V1Clp1        reade     TITAS37C                               24
150700000717     c                   ENDIF
150800000717      *
150900000717     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
151000000717     C                   exsr      CTR_selez
151100000717      * esco per destinatario non trovato
151200000717     c                   If        %SUBST(V1CRSD:1:XX) <> %SUBST(TASRSD:1:XX)
151300000717     C                   eval      *IN24 = *on
151400000717     c                   Else
151500000717     C     V1Clp1        reade     TITAS37C                               24
151600000717     c                   Endif
151700000717     C                   ENDDO
151800000717      *
151900000717     c                   ENDSR
152000000717      *****************************************************************
152100000718      * CARICAMENTO SUBFILE * SELEZIONE 12 - lnp + data + serie
152200000717      *****************************************************************
152300000717     C     RIESEL12      BEGSR
152400000718      *
152500000718     c                   IF        *IN23 = *OFF
152600000718     c                   movel     WV1Cdsd       Kaas
152700000718     c                   move      WV1Cdsd       Kmgs
152800000718     C                   move      V1Cser        WorkSerie
152900010112     C                   eval      klinea = v1clp1
153000000718     C     leggititas1   tag
153100000718     C     kdta1         setll     TITAS39C
153200000718     C     kdta1         reade     TITAS39C                               24
153300000718     C     *in24         ifeq      *on
153400000718     C     kdta          setgt     titas39c
153500000718     C     V1Clp1        reade     titas39c                               24
153600000718     C   24              goto      endsbr12
153700000718     c                   eval      Kaas = TASaas
153800000718     c                   eval      Kmgs = TASmgs
153900000718     C                   goto      leggititas1
154000000718     C                   endif
154100000718     C                   ENDIF
154200000718      *
154300000718     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
154400000718     C                   exsr      CTR_selez
154500000718     C     wdatasp       ifle      WV1Cdsa
154600000718     C     leggititas2   tag
154700000718     C     Kdta1         reade     TITAS39C                               24
154800000718     C     *in24         ifeq      *on
154900000718     C     kdta          setgt     titas39c
155000000718     C     V1Clp1        reade     titas39c                               24
155100000718     C   24              goto      endsbr12
155200000718     c                   eval      Kaas = TASaas
155300000718     c                   eval      Kmgs = TASmgs
155400000718     C     kdta1         setll     TITAS39C
155500000718     C                   goto      leggititas2
155600000718     C                   endif
155700000718     C                   else
155800000718     C                   seton                                        24
155900000718     C                   endif
156000000718     C                   ENDDO
156100000718      *
156200000718     c     endsbr12      ENDSR
156300000717      *****************************************************************
156400010104      * CARICAMENTO SUBFILE * SELEZIONE 13 - segnacollo
156500000717      *****************************************************************
156600000717     C     RIESEL13      BEGSR
156700010111
156800010112     c                   clear                   bolle
156900010112     C                   eval      *in15 = (v1clna > *zeros)
157000010112
157100010129      * Segnacollo univoco (segnacollo da TITAS o presente in TITAT)
157200010112     C                   if        v1flgs <> *blanks
157300010111
157400010109      * Lettura su TITAS (segnacolli sequenziali)
157500010112     C   15Ktat          setll     titas34c
157600010112     C  n15ktat1         setll     titas34c
157700010111     C                   do        *hival
157800010112     C   15Ktat          reade     titas34c
157900010112     C  n15ktat1         reade     titas34c
158000010111
158100010111     C                   if        %eof(titas34c) or wendpag = 'S'
158200010111     C                   leave
158300010111     C                   endif
158400010112      * Selezioni
158500010104     C                   exsr      CTR_selez
158600010111
158700010129      * Carico una schiera con le bolle che trovo
158800010129     C                   if        *in32 = *on
158900010111     c     dsbolle       lookup    bolle                                  30
159000010111     c                   IF        *IN30 = *off
159100010111     c                   eval      xb = 1
159200010111     c     *blanks       lookup    bolle(xb)                              30
159300010111     c   30              eval      bolle(xb) = dsbolle
159400010111     c                   ENDIF
159500010112     C                   endif
159600010111
159700010111     C                   enddo
159800010112
159900010129      * Lettura su TITAT
160000010129      * segnacolli non sequenziali o sequenziali con data consegna <> da TAS
160100010112     c   15ktat          setll     titat24c
160200010112     c  n15ktat1         setll     titat24c
160300010109     C                   do        *hival
160400010112     C   15ktat          reade     titat24c
160500010112     C  n15ktat1         reade     titat24c
160600010112     C                   if        %eof(titat24c) or Wendpag = 'S'
160700010109     C                   leave
160800010109     C                   endif
160900010112     C     ktats         chain     titas30c
161000010109     C                   if        %found(titas30c)
161100010112      * selezioni
161200010111     C                   exsr      CTR_selez
161300010111
161400010109      * Controllo se ho già scritto la bolla
161500010112     c   32dsbolle       lookup    bolle                                  30
161600010112     C   30              iter
161700010109
161800010112     C                   endif
161900010129     C                   enddo
162000010111
162100010111     C                   endif
162200010111
162300010129      * Segnacollo non univoco
162400010129      * Sequenziali: compreso tra segnac. da e segnac. a in TITAS
162500010129      * Non Sequenziali: presente in TITAT
162600010112     C                   if        v1flgs = *blanks
162700010111
162800010111      * Lettura su TITAS
162900010115     c                   IF        *IN23 = *OFF
163000010112     c                   movel     WV1cdsd       Kaas
163100010112     c                   move      WV1cdsd       Kmgs
163200010112     C                   move      V1Cnrr        WorkSerie
163300010112     C                   move      V1Cnsc        KSEGNA
163400010112     C                   eval      klinea = v1cfls
163500010112     C     leggitita     tag
163600010112     C     kdta2         setgt     TITAS35C
163700010112     C     kdta1         readpe    TITAS35C                               24
163800010111     C     *in24         ifeq      *on
163900010112     C     kdta          setgt     titas35c
164000010112     C     V1Cfls        reade     titas35c                               24
164100010115     C   24              goto      leggititat
164200010111     c                   eval      Kaas = TASaas
164300010111     c                   eval      Kmgs = TASmgs
164400010112     C                   goto      leggitita
164500010111     C                   endif
164600010115     C                   endif
164700010111      *
164800010111     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
164900010115     C                   if        tasncd > *zeros
165000010111     C                   exsr      CTR_selez
165100010129      * Carico una schiera con le bolle che trovo.
165200010129      * Solo serie > 0 perchè elaboro TITAT
165300010112     C                   if        *in32 = *on  and v1cnrr > *zeros
165400010112     c     dsbolle       lookup    bolle                                  30
165500010112     c                   IF        *IN30 = *off
165600010112     c                   eval      xb = 1
165700010112     c     *blanks       lookup    bolle(xb)                              30
165800010112     c   30              eval      bolle(xb) = dsbolle
165900010112     c                   ENDIF
166000010112     C                   endif
166100010115     C                   endif
166200010112
166300010112     C     wdatasp       ifle      WV1cdsa
166400010112     C     leggitita1    tag
166500010112     C     Kdta1         readpe    TITAS35C                               24
166600010111     C     *in24         ifeq      *on
166700010112     C     kdta          setgt     titas35c
166800010112     C     V1Cfls        reade     titas35c                               24
166900010115     C   24              goto      leggititat
167000010111     c                   eval      Kaas = TASaas
167100010111     c                   eval      Kmgs = TASmgs
167200010112     C     kdta2         setgt     TITAS35C
167300010112     C                   goto      leggitita1
167400010111     C                   endif
167500010111     C                   else
167600010111     C                   seton                                        24
167700010111     C                   endif
167800010111     C                   ENDDO
167900010112
168000010115     C     leggititat    tag
168100010112      * Lettura su TITAT
168200010129      * Per semplicità di pgm leggo anche eventuali segnac. sequenziali con
168300010129      * serie e data cons. <> da bolla che ho gia elaborato prima.
168400010112     C                   if        v1cnrr > *zeros
168500010112     c   15ktat          setll     titat24c
168600010112     c  n15ktat1         setll     titat24c
168700010112     C                   do        *hival
168800010112     C   15ktat          reade     titat24c
168900010112     C  n15ktat1         reade     titat24c
169000010112     C                   if        %eof(titat24c) or Wendpag = 'S'
169100010112     C                   leave
169200010112     C                   endif
169300010112     C     ktats         chain     titas30c
169400010112     C                   if        %found(titas30c)
169500010112      * selezioni
169600010112     C                   exsr      CTR_selez
169700010112      * Controllo se ho già scritto la bolla
169800010112     c   32dsbolle       lookup    bolle                                  30
169900010112     C   30              iter
170000010112
170100010112     C                   endif
170200010112     C                   enddo
170300010112     C                   endif
170400010111
170500010111     C                   endif
170600010111
170700000717      *
170800010115     c                   ENDSR
170900000901      *****************************************************************
171000000901      * CARICAMENTO SUBFILE * SELEZIONE 14 - Codice "CHI SONO"
171100000901      *****************************************************************
171200000901     C     RIESEL14      BEGSR
171300000901      *
171400000901     c                   IF        *IN23 = *OFF
171500000901     C     ktah          chain     TITAH31C                           33
171600000901     C  N33ktahxs        chain     TITAS30C                           24
171700000901     c                   ENDIF
171800000901      *
171900000901     c                   DOW       *in33=*Off
172000000901     c                   DOW       *in24=*Off  and  Wendpag <> 'S'
172100000901     C                   exsr      CTR_selez
172200040217     c     Wendpag       cabeq     'S'           EndSel14
172300000901     C     ktahxs        reade     TITAS30C                               24
172400000901     C                   ENDDO
172500040217     c     Wendpag       cabeq     'S'           EndSel14
172600000901     C     ktah          reade     TITAH31C                               33
172700000901     C  N33ktahxs        chain     TITAS30C                           24
172800000901     C                   ENDDO
172900000901      *
173000040217     c     EndSel14      ENDSR
173100010219      *****************************************************************
173200010219      * CARICAMENTO SUBFILE * SELEZIONE 15 - numero ORM
173300010219      *****************************************************************
173400010219     C     RIESEL15      BEGSR
173500010219
173600010219      * Ricerco ORM in TITA4
173700010219
173800010219     c                   IF        *IN23 = *OFF
173900010219     C     ds_orm        chain     TITA432C                           33
174000010219     C  N33Ktas          chain     TITAS30C                           24
174100010219     c                   ENDIF
174200010219      *
174300010219     c                   DOW       *IN33 = *off
174400010219     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
174500010219     C                   exsr      CTR_selez
174600040217     c     Wendpag       cabeq     'S'           EndSel15
174700010219     C     Ktas          reade     TITAS30C                               24
174800010219     C                   ENDDO
174900040217     c     Wendpag       cabeq     'S'           EndSel15
175000010219     C     ds_orm        reade     TITA432C                               33
175100010219     C  N33Ktas          chain     TITAS30C                           24
175200010219     C                   ENDDO
175300010219
175400040217     C     EndSel15      ENDSR
175500000223      *****************************************************************
175600000223      * CONTROLLO SE SPEDIZIONE DPD NON ANCORA SELZIONATA
175700000223      *****************************************************************
175800000223     C     CTRspeddpd    BEGSR
175900000223      *
176000000223     c     TASlnp        lookup    FILDPD                                 30
176100000223     c  N30TASlna        lookup    FILDPD                                 30
176200000223      *
176300000223     c                   IF        *IN30 = *OFF
176400000223     c                   eval        *in31 = *on
176500000223     C                   ELSE
176600000223      *
176700000223     c                   eval      DSaas = TASaas
176800000223     c                   eval      DSlnp = TASlnp
176900000223     c                   eval      DSnrs = TASnrs
177000000223     c                   eval      DSnsp = TASnsp
177100000223      *
177200000223     c     DSsped        lookup    SPED                                   31
177300000223     c                   IF        *IN31 = *off
177400000223     c                   eval      xx = 1
177500000223     c     *zeros        lookup    SPED(xx)                               30
177600000223     c                   eval      SPED(xx) = DSsped
177700000223     c                   ENDIF
177800000223      *
177900000223     c                   ENDIF
178000000223      *
178100000223     c                   ENDSR
178200040216      *****************************************************************
178300040216      * CARICAMENTO SUBFILE * SELEZIONE 16 - Rif. alfabetico
178400040216      *****************************************************************
178500040216     C     RIESEL16      BEGSR
178600040216      *
178700040216     c                   IF        *IN23 = *OFF
178800040216     C     V1Crma        chain     TITA435C                           33
178900040216     C  N33Ktas          chain     TITAS30C                           24
179000040216     c                   ENDIF
179100040216      *
179200040216     c                   DOW       *IN33 = *off
179300040216     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
179400040216     C                   exsr      CTR_selez
179500040217     c     Wendpag       cabeq     'S'           EndSel16
179600040216     C     Ktas          reade     TITAS30C                               24
179700040216     C                   ENDDO
179800040217     c     Wendpag       cabeq     'S'           EndSel16
179900040216     C     V1Crma        reade     TITA435C                               33
180000040216     C  N33Ktas          chain     TITAS30C                           24
180100040216     C                   ENDDO
180200040216      *
180300040217     c     EndSel16      ENDSR
180400990618      *****************************************************************
180500990618      * CONTROLLO SE C.A. SELEZIONABILE PER CARICAMENTO SUBFILE
180600990618      *****************************************************************
180700990618     C     CTR_selez     BEGSR
180800990618
180900010111     C                   eval      *in32 = *off
181000990623      * Compongo la data spedizione per confronti
181100990623     C                   eval      wdatasp = (TASaas * 10000) + TASmgs
181200990623
181300990622      * determino lunghezza stringa ricerca destinatario
181400990622     C                   IF        V1Crsd <> *blanks
181500990622     c     ' '           checkr    V1Crsd        xx
181600990622     c                   ENDIF
181700990707
181800990707      * imposto segnacollo finale se zero
181900010112     C                   IF        TASnca = *zeros
182000010112     c                   z-add     TASncd        TASnca
182100010112     c                   ENDIF
182200000717      * imposto serie
182300000717     C                   IF        V1Cser <> *blanks
182400000717     c                   move      V1Cser        WorkSerie         2 0
182500000717     c                   ENDIF
182600000717
182700010129      * Bolle SI/NO/SOLO Poste
182800010129      * Non controllo se P.O. partenza o P.O. segnacollo impostati
182900010129      * e se accetto tutte le bolle (flag = "S")
183000010129     C                   eval      *in37 = *off
183100010129      *
183200010129     c                   IF        V1Cfls = *zeros  and  V1Clp1 = *zeros
183300030529     c                                              and  V1Csnp <> 'S'
183400010129     C     TASlnp        lookup    FILPOSTE                               30
183500000717      *
183600010129     c                   IF        *IN30 = *on  and  V1Csnp = 'N'
183700010129     C                   eval        *in37 = *on
183800010129     C                   ENDIF
183900010129      *
184000010129     c                   IF        *IN30 = *off  and  V1Csnp = 'P'
184100010129     C                   eval        *in37 = *on
184200010129     C                   ENDIF
184300000717      *
184400010129     C                   ENDIF
184500030529     C*
184600030529     C* Inserisco qui i controlli x verificare la data limite x considerare le
184700030529     C* bolle PT (tale controllo nn viene effettuato se l'utente è del gruppo
184800030529     C* EDP o ISP).
184900030529     C                   if        %subst(KPJBU:2:2) <> '05'
185000030529     C                   if        %subst(KNMUS:1:3) = 'EDP'
185100030529     C                   else
185200030529     C     tasLNP        lookup    FILPOSTE                               96
185300030529     C                   if        *in96
185400030529     C                   if        tasDCM >  0
185500030529     C                   if        tasDCM >= wDataLimitePT
185600030529     C                   else
185700030529     C                   seton                                        37
185800030529     C                   endif
185900030529     C                   endif
186000030529     C                   endif
186100030529     C                   endif
186200030529     C                   endif
186300030529     C*
186400990622
186500990618     c                   SELECT
186600990618      *
186700010129     C                   WHEN      *in37 = *on                                  POSTE SI/NO
186800000717      *
186900000717     C                   WHEN      V1Cksc <> *zeros  AND  V1Cksc <> TASksc      CODICE FATTURAZIONE
187000990618      *
187100990622     C                   WHEN      WV1Cdsd > *zeros  AND                        DATA SPEDIZIONE
187200990623     c                             (Wdatasp < WV1Cdsd or Wdatasp > WV1Cdsa)
187300990618      *
187400990622     C                   WHEN      V1Crsd <> *blanks  AND                       DESTINATARIO
187500990622     c                             %SUBST(V1CRSD:1:XX) <> %SUBST(TASRSD:1:XX)
187600990618      *
187700990622     C                   WHEN      V1Clp1 > *zeros  AND  V1Clp1 <> TASlnp       P.O. PARTENZA
187800000717      *
187900000717     C                   WHEN      V1Cser <> *blanks AND WorkSerie <> TASnrs    SERIE
188000990622      *
188100990622     C                   WHEN      V1Clna > *zeros  AND  V1Clna <> TASlna       P.O. ARRIVO
188200040216      *
188300040216     C                   WHEN      V1Ccbo > *blanks AND  V1Ccbo <> TAScbo       CODICE BOLLA
188400990623      *
188500990623      * I controlli per segnacollo devono restare alla fine
188600010112     C                   WHEN      V1Cnsc > *zeros  AND                          SELEZ. SEGNACOLLO
188700010112     C                               (V1Cfls <> TASfls  OR  V1Cnrr <> TASnrs)   P.O. E SERIE SEGNAC.
188800990618      *
188900010115     C                   WHEN      V1Cnsc > *zeros  AND  tasfns <> 'S'  AND        NUMERO SEGNACOLLO
189000010112     C                             TASncd > *zeros  AND  V1Cnsc < TASncd                 DAL
189100990623      *
189200010115     C                   WHEN      V1Cnsc > *zeros  AND  tasfns <> 'S'  AND        NUMERO SEGNACOLLO
189300010112     C                             TASnca > *zeros  AND  V1Cnsc > TASnca                AL
189400010115      *
189500010115     C                   WHEN      V1Cnsc > *zeros  AND  tasfns <> 'S'  and        NUMERO SEGNACOLLO
189600010115     C                             (V1Cnsc > TASnca  OR  V1Cnsc < TASncd)               AL
189700990618      *
189800990618     c                   OTHER
189900990623      *
190000010111      * indicatore di comodo per caricare la schiera nella riesel13
190100010115     C                   eval      *in32 = *on
190200010115     C                   exsr      RICsbfl
190300990618      *
190400990618     C                   ENDSL
190500990618      *
190600990618     c                   ENDSR
190700990623      **********************************************************************
190800990623      * SCRITTURA RCD SBFL
190900990623      **********************************************************************
191000990623     C     RICsbfl       BEGSR
191100120517      *
191200120517      * - Già carico il n° max di rec. nel sfl !!!
191300120517     c                   if        Nrr1 = *hival
191400120517     c                   eval      $Full = *on
191500120517     c                   eval      Wendpag = 'S'
191600120517     c                   eval      V1Cmsg  = Msg(40)
191700120517     c                   eval      *in90 = *on
191800120517     c                   eval      *in28 = *on
191900120517     c                   leavesr
192000120517     c                   endif
192100990623      *
192200990618      * Carico
192300990622     c                   movel     *blanks       V2Copz
192400990622     c                   z-add     TASaas        V2Haas
192500990623     c                   z-add     TASaas        V2Cdsp
192600990623     c                   movel     TASmgs        V2Cdsp
192700990623     c     *mdy          move      V2Cdsp        DATA_eur
192800990623     c     *dmy          move      DATA_eur      V2Cdsp
192900990622     c                   z-add     TASlnp        V2Clnp
193000990622     c                   z-add     TASnrs        V2Cnrs
193100990622     c                   z-add     TASnsp        V2Cnsp
193200990622     c                   movel     TAStbl        V2Ctbl
193300990622     c                   z-add     TASlna        V2Clna
193400990622     c                   z-add     TASnft        V2Cnft
193500990622     c                   IF        TASdft = *zeros
193600990707     c                   clear                   V2Cdft
193700990622     c                   ELSE
193800990624     c     *iso          move      TASdft        DATA_eur
193900990707     c     *dmy          move      DATA_eur      W0060
194000990707     C                   eval      V2Cdft = %EDITC(W0060:'Y')
194100990622     c                   ENDIF
194200990622     c                   z-add     TASrmn        V2Crmn
194300990622     c                   z-add     TASksc        V2Cksc
194400990622     c                   z-add     TASctr        V2Cctr
194500990622     c                   movel     TAStsp        V2Ctsp
194600990622     c                   movel     TASrsd        V2Crsd
194700990707     c                   IF        TASdcm = *zeros
194800990707     c                   clear                   V2Cdcm
194900990707     c                   ELSE
195000990707     c     *iso          move      TASdcm        DATA_eur
195100990707     c     *eur          move      DATA_eur      W0080
195200990707     C                   eval      V2Cdcm = %EDITC(W0080:'Y')
195300990707     c                   ENDIF
195400990618      *
195500990622      * Ricavo ragione sociale mittente
195600990622     c                   clear                   V2Crsc
195700990708     c                   move      TASccm        W0040
195800990623     c                   IF        W0040 = 9999  or  W0040 = 8888
195900990623     c                                           or  W0040 = *zeros
196000990629     c                   eval      ktrc = 'M'
196100990707     c     KTAA          CHAIN     TITAA30C
196200990707     c                   IF        %FOUND
196300990622     c                   movel     TAArsc        V2Crsc
196400990618     c                   ENDIF
196500990622      *
196600990622     c                   ELSE
196700990622     c                   z-add     TASccm        I69kac
196800990622     c                   exsr      CTR_anagra
196900990707     C                   If         WO69ERR <> '1'
197000990622     c                   movel     ACOrag        V2Crsc
197100990622     c                   Endif
197200990622     c                   ENDIF
197300990618      *
197400990623     c                   add       1             NUMspedcar
197500990618     c                   add       1             NRR1
197600990623     c                   add       1             Wspedpag
197700990618      *
197800990622      * posizione cursore pagina nuova per ROLLUP carico 18 rcd per volta
197900990618     c                   SELECT
198000990623     c                   WHEN      Wspedpag = 1
198100990618     c                   eval      nrr23 = nrr1
198200990623     c                   WHEN      Wspedpag = 18
198300990618     c                   eval      Wendpag = 'S'
198400990618     c                   ENDSL
198500990618      *
198600990623     c                   write     SB50S02
198700990618      *
198800990622     c                   ENDSR
198900990618      **********************************************************************
199000990618      * EMISSIONE VIDEO 2
199100990618      **********************************************************************
199200990618     C     EMISD02       BEGSR
199300990618      *
199400990618     c                   SELECT
199500990618     c                   WHEN      *IN23 = *ON  and NRR23 > *zeros
199600990618     c                   z-add     nrr23         V2Cnrr
199700990618     c                   WHEN      *IN90 = *ON  and NRR1 > *zeros
199800990618     c                   z-add     nrr1          V2Cnrr
199900990618     c                   OTHER
200000990618     c                   z-add     1             V2Cnrr
200100990618     c                   ENDSL
200200990618      *
200300990730     c                   eval      *in20 = *on
200400990730     c                   eval      *in21 = *on
200500990618      *
200600990623     c                   write     SB50T01
200700990623     c                   write     SB50Z02
200800990623     c                   exfmt     SB50C02
200900990618      *
201000990730     c                   eval      *in90 = *off
201100990730     c                   eval      *in28 = *off
201200990623     c                   clear                   v1cmsg
201300990618      *
201400990618     C                   ENDSR
201500990618      **********************************************************************
201600990618      * CONTROLLI VIDEO 2
201700990618      **********************************************************************
201800990618     C     Ctrd02        BEGSR
201900990618      *
202000990624     c                   clear                   wselsped
202100990802     c                   clear                   nrr60
202200990618     c                   z-add     1             nrr2
202300990618      *
202400990706     c     NRR2          CHAIN     SB50S02
202500990618      *
202600990706     c                   DOW       %FOUND  and  *IN90 = *OFF
202700990618
202800990618     c                   SELECT
202900990618
203000990630     c                   WHEN      V2Copz = *blanks
203100990630
203200990630     c                   WHEN      V2Copz = '5'
203300990623     c                   exsr      SUB_opz05
203400990623     c                   clear                   V2Copz
203500990706     c                   eval      *in90 = (O51F03 = '1')                       per F3 torno al SFL
203600990802     c                   eval      *in60 = *on
203700990802     c                   z-add     NRR1          NRR60
203800041012      * se selezionata bolla vado a fine
203900110520     c                   if        not *in90
204000110520     c                   eval      *IN90 = (wselsped = '1' and o51f03 <> *on)
204100110520     c                   endif
204200990802
204300990705     c                   WHEN      V2Copz = '1'  and  V2Copz = V2Dsel           Selezione
204400990624     c                   z-add     V2Haas        D50aas
204500990624     c                   z-add     V2Clnp        D50lnp
204600990624     c                   z-add     V2Cnrs        D50nrs
204700990624     c                   z-add     V2Cnsp        D50nsp
204800990624     c                   movel     V2Ctbl        D50tbl
204900041008      * se richiamato dai reclami
205000041008     c                   If        sdsprm > 1
205100041008     c                   eval      IA1ins = 'S'
205200041008     c                   eval      IA1tor = 'S'
205300041008     c                   eval      IA1ogg = ds_OggSped
205400041008e   3c                   endif
205500041008      *
205600990624     c                   eval      *IN90 = *ON
205700990624     c                   eval      wselsped = '1'
205800990705
205900990705     c                   WHEN      V2Copz = '2'  and  V2Copz = V2Dsel           Gestione
206000990726     c                   exsr      SUB_opz02
206100990726     c                   clear                   V2Copz
206200990726     c                   eval      *in90 = (D01F03 = '1')                       per F3 torno al SFL
206300990802     c                   eval      *in60 = *on
206400990802     c                   z-add     NRR1          NRR60
206500990726     c                   if        D01msg <> *blanks
206600990726     c                   movel     D01msg        V1Cmsg
206700990730     c                   eval      *in90 = *on
206800990730     c                   eval      *in28 = *on
206900990726     c                   endif
207000990618
207100990623     c                   OTHER                                                  Opzione errata
207200990623     c                   movel     MSG(20)       V1Cmsg
207300990730     c                   eval      *in28 = *on
207400990802     c                   eval      *in90 = *on
207500990730     c                   eval      *in60 = *on
207600990802     c                   z-add     NRR1          NRR60
207700990618
207800990618     c                   ENDSL
207900990618      *
208000990623     c                   update    SB50S02
208100990802     c                   eval      *IN60 = *OFF
208200990618      *
208300990618     c                   IF        *IN90 = *OFF
208400990618     c                   add       1             nrr2
208500990706     c     NRR2          CHAIN     SB50S02
208600990618     c                   ENDIF
208700990618      *
208800990618     c                   enddo
208900990618      *
209000990802      * Se devo posizionarmi senza errore accendo IN90
209100990802     c                   IF        NRR60 <> *zeros
209200990802     c                   z-add     NRR60         NRR1
209300990802     c                   eval      *IN90 = *on
209400990802     c                   ENDIF
209500990802      *
209600990618     C                   ENDSR
209700990618      **********************************************************************
209800990623      * PREPARAZIONE DS PASSAGGIO PARAMETRI PER SELEZIONE 5
209900990618      **********************************************************************
210000990623     C     SUB_OPZ05     BEGSR
210100990618      *
210200990624     c                   clear                   TNSB51DS
210300990624      *
210400990705     c                   movel     I50OP0        I51OP0
210500990705     c                   movel     I50OP1        I51OP1
210600990705     c                   z-add     V2Haas        I51aas
210700990624     c                   z-add     V2Clnp        I51lnp
210800990624     c                   z-add     V2Cnrs        I51nrs
210900990624     c                   z-add     V2Cnsp        I51nsp
211000990624     c                   movel     V2Ctbl        I51tbl
211100990618      *
211200990624     C                   movel     TNSB51DS      Kpjbu
211300041008      * se richiamato dai recalmi passo anche
211400041008     c                   if         SDSprm > 1
211500041008      *
211600990624     c                   call      'TNSB51R'
211700990624     c                   parm                    KPJBA
211800041008     c                   parm                    FIDNA1ds
211900041012      *
212000041012      * se richiamato da reclami e selezionato la bolla vado a fine
212100041012if  1c                   if            SDSprm > 1
212200041012     c                             and IA1ins = 'S'
212300041012     c                   eval      Wselsped = '1'
212400041012e   1c                   endif
212500041008      *
212600041008     c                   else
212700041008      *
212800041008     c                   call      'TNSB51R'
212900041008     c                   parm                    KPJBA
213000041008      *
213100041008     c                   endif
213200990624      *
213300990624     C                   movel     KPJBU         TNSB51DS
213400990624      *
213500990618     C                   ENDSR
213600990726      **********************************************************************
213700990726      * PREPARAZIONE DS PASSAGGIO PARAMETRI PER SELEZIONE 2
213800990726      **********************************************************************
213900990726     C     SUB_OPZ02     BEGSR
214000990726      *
214100990726     c                   clear                   TNSB01DS
214200990726      *
214300990726     c                   movel     ' 02'         D01OP0
214400990726     c                   z-add     V2Haas        D01aas
214500990726     c                   z-add     V2Clnp        D01lnp
214600990726     c                   z-add     V2Cnrs        D01nrs
214700990726     c                   z-add     V2Cnsp        D01nsp
214800990726     c                   movel     V2Ctbl        D01tbl
214900990726      *
215000990726     C                   movel(P)  TNSB01DS      Kpjbu
215100990726      *
215200990726     c                   call      'TNSB52R'
215300990726     c                   parm                    KPJBA
215400990726      *
215500990726     C                   movel     KPJBU         TNSB01DS
215600990726      *
215700990726     C                   ENDSR
215800990621      *****************************************************************
215900990621      *  AGGANCIO ANAGRAFICHE
216000990621      *****************************************************************
216100990621     C     CTR_ANAGRA    BEGSR
216200990621      *
216300990707     c                   clear                   WO69ERR
216400990707      *
216500990621     C                   CALL      'TIBS69R'
216600990621     C                   PARM                    tibs69DS
216700990621     C                   PARM                    DS_cnaco
216800990621     C                   PARM                    DS_cnind
216900990621     C                   PARM                    DS_cnclp
217000990621     C                   PARM                    DS_fncls
217100990621      *
217200990621     C                   eval      wtibs69r = 'S'
217300990707     C                   eval      WO69ERR =  O69ERR
217400990621      * per il 1° giro è inizializzata nelle specifiche "D"
217500990621     C                   clear                   TIBS69DS
217600990621      *
217700990621     C                   ENDSR
217800990618      *****************************************************************
217900990618      *  Ricerca alfabetica
218000990618      *****************************************************************
218100990618     c     ricalf        begsr
218200990618      *
218300990618     C                   MOVEL     rsut          PAXdut                         Descrizione Azienda
218400990618     C                   Z-ADD     9             PAXsta                         9=Escludo Annullati
218500030529     C                   Z-ADD     DUTKCI        PAXccc                         Capoconto Clienti
218600000901     C                   z-add     1             PAXNUM
218700990618      *
218800000901     C                   CALL      'XALFA3BR'
218900990618     C                   PARM                    PAXDUT
219000990618     C                   PARM                    CODUT
219100990618     C                   PARM                    PAXDMT
219200990618     C                   PARM                    PAXCCC
219300990618     C                   PARM                    PAXSTA
219400990618     C                   PARM      *blanks       PAXFLR
219500990618     C                   PARM      *blanks       PAXDIT
219600000901     C                   PARM                    PAXNUM            2 0
219700000901     C                   PARM                    PAXKCM           80
219800000901     C                   PARM                    PAXKSM          140
219900000901     C                   PARM                    PAXKDM           60
220000990618      *
220100990618     c                   endsr
220200990805      **********************************************************************
220300990805      * CONTROLLI STANDARD    * data immessa a video *
220400990805      **********************************************************************
220500990805     C     CTR_data      BEGSR
220600990805      *
220700990805     c     *eur          TEST(DE)                WDATA_08
220800990805     c                   IF        %error = *off
220900990805     c     *eur          movel     WDATA_08      DATA_EUR
221000990805     c     *iso          movel     DATA_eur      WDATA_08G
221100990805     c                   ELSE
221200990805     c     *dmy          TEST(DE)                WDATA_08
221300990805     c                   IF        %error = *off  and  WDATA_02 = *zeros
221400990805     c     *dmy          movel     WDATA_06      DATA_EUR
221500990805     c     *eur          movel     DATA_eur      WDATA_08
221600990805     c     *iso          movel     DATA_eur      WDATA_08G
221700990805     c                   ELSE
221800990805     c                   eval      *IN28 = *on
221900990805     c                   ENDIF
222000990805     c                   ENDIF
222100990805      *
222200990805     c                   ENDSR
222300990618      **********************************************************************
222400990618      * CONTROLLI STANDARD    * linea *
222500990618      **********************************************************************
222600990618     C     CTR_linea     BEGSR
222700990618      *
222800990623     C                   IF        Wlinea = *zeros
222900990618     c                   MOVEL     MSG(6)        v1CMSG
223000990618     c                   eval      *in28 = *on
223100990618     c                   eval      *in90 = *on
223200990618     c                   ELSE
223300990618      *
223400990707     c     Wlinea        chain     azorg01l
223500990707     c                   if        not %FOUND  or  ORGfva <> *blanks
223600990618     c                   MOVEL     MSG(7)        v1CMSG
223700990618     c                   eval      *in28 = *on
223800990618     c                   eval      *in90 = *on
223900990618     c                   endif
224000990618     c                   ENDIF
224100990618      *
224200990618     c                   ENDSR
224300990618      **********************************************************************
224400990618      * CONTROLLI STANDARD    * anno *
224500990618      **********************************************************************
224600990618     C     CTR_anno      BEGSR
224700990618      *
224800990623     c                   IF        Wanno = *zeros
224900990618     c                   MOVEL     MSG(9)        v1CMSG
225000990730     c                   eval      *in90 = *on
225100990730     c                   eval      *in28 = *on
225200990618     C                   else
225300990618      *
225400990618      *   sistemo anno di due cifre
225500990623     C                   if        Wanno < 100  and Wanno > 60
225600990623     C                   ADD       1900          Wanno
225700990618     C                   endif
225800990623     C                   if        Wanno < 100  and Wanno <= 60
225900990623     C                   ADD       2000          Wanno
226000990618     C                   endif
226100990618      *
226200990618     C                   ENDIF
226300990618      *
226400990618     c                   ENDSR
226500030529     C*--------------------------------------------------------------------------------------------*
226600030529     C* CARTAB - CARICA I DATI TABELLATI
226700030529     C*--------------------------------------------------------------------------------------------*
226800030529     C     CARTAB        BEGSR
226900030529     C*
227000030529     C* Partendo dalla data attuale reperisco la data limite olre la quale
227100030529     C* nn scendere nel considerare le bolle PT.
227200030529     C                   eval      wrkoggiiso = *loval
227300030529     C*
227400030529     C                   eval      tbeCOD = 'VPO'
227500030529     C                   eval      tbeKE1 = 'VPO'
227600030529     C     KTBE01P       chain     TNTBE01L
227700030529     C                   if        %found(TNTBE01L)
227800030529     C                   eval      DVPO = tbeUNI
227900030529     C*
228000030529     C* A seconda che l'utente sia di SEDE o d Filiale considero due valori diversi x i giorni
228100030529     C* dalla data attuale entro i quali considerare le bolle PT >>> CONSEGNATE <<<
228200031114      * l'utente ASC99 è inserito come utente di sede, ma di fatto è un utente di filiale
228300031114     C                   if        DUTPOU <> 046 or Knmus = 'ASC99     '
228400030529     C                   eval      wGGLimPT = §VPOIPP
228500030529     C                   else
228600030529     C                   eval      wGGLimPT = §VPOIPS
228700030529     C                   endif
228800030529     C*
228900030529     C* Se il valore in tabella è 9999 significa nessun blocco data limite
229000030529     C                   if        wGGLimPT <> 9999
229100030529     C                   time                    wrkoggiiso
229200030529     C     wrkoggiiso    subdur    wGGLimPT:*DAYSwrkoggiiso
229300030529     C                   endif
229400030529     C                   endif
229500030529     C     *iso          move      wrkoggiiso    wDataLimitePT
229600030529     C*
229700030529     C                   ENDSR
229800030529     C*--------------------------------------------------------------------------------------------*
229900030529     C* REPDATIUTE - REPERISCE DATI UTENTE
230000030529     C*--------------------------------------------------------------------------------------------*
230100030529     C     REPDATIUTE    BEGSR
230200030529     C*
230300030529     C* Reimposta le variabili di lavoro
230400030529     C                   CLEAR                   TIBS34DS
230500030529     C                   CLEAR                   AZUTEDS
230600030529     C                   CLEAR                   DDATIUTE
230700030529     C*
230800030529     C* Reperisce i dati del profilo in ingresso
230900030529     C     *DTAARA       DEFINE    §azute        azuteds
231000030529     C     *DTAARA       DEFINE    §datiute      ddatiute
231100030529     C                   IN(E)     *DTAARA
231200030529     C                   IF        %Error                                       *NO errori
231300030529     C                   EVAL      I34Tla = 'L'
231400030529     C                   CALL      'TIBS34R'
231500030529     C                   PARM                    TIBS34DS
231600030529     C                   IN        *DTAARA
231700030529     C                   ENDIF
231800030529     C*
231900030529     C                   ENDSR
232000990618      *****************************************************************
232100990618      * ROUTINE INIZIALE
232200990618      *****************************************************************
232300990618     C     *INZSR        BEGSR
232400990618      *
232500990618     C     *ENTRY        PLIST
232600990618     C                   PARM                    KPJBA
232700041008     C                   PARM                    fidna1ds
232800030529     C*
232900030529     C* REPERISCE I DATI UTENTE
233000030529     C                   EXSR      REPDATIUTE
233100030529     C*
233200030529     C* CARICA I DATI TABELLATI
233300030529     C                   EXSR      CARTAB
233400000718      * ! metodo
233500000718     C                   z-add     2             metodo            1 0
233600000718      *
233700990624      *--- DATI UTENTE/AZIENDA
233800030529     C                   Z-ADD     1             CODUT             1 0
233900000223
234000010129      * Carico schiera con P.O. DPD e P.O. POSTE
234100000223     c                   clear                   xx
234200000223     C     *loval        setll     azorg
234300000223     C                   read      azorg
234400000223      *
234500000223     C                   DOW       NOT %EOF
234600000223      *
234700010129     c                   movel     ORGde3        OG143
234800010129      *
234900010129     c                   SELECT
235000010129     c                   WHEN      ORGFVA <> *blanks
235100020211     C                   WHEN      §ogntw = 'DPD'
235200000223     C                   add       1             XX
235300000223     c                   z-add     ORGFIL        FILDPD(XX)
235400020211     C                   WHEN      §ogntw= 'PPT'
235500010129     C                   add       1             YY
235600010129     c                   z-add     ORGFIL        FILPOSTE(YY)
235700010129     C                   ENDSL
235800000223      *
235900000223     C                   read      azorg
236000000223     C                   ENDDO
236100990618
236200990618      * reperisco data e ora
236300990618     C                   TIME                    W0140
236400990618      * UDATE IN GGMMAAAA
236500990618     C                   MOVE      W0140         WDTGIO
236600990618      * UDATE IN AAAAMMGG
236700990805     C     *eur          MOVEL     WDTGIO        DATA_eur
236800990805     C     *iso          MOVEL     DATA_eur      dateu
236900990618
237000990618     C     kspe          KLIST
237100990622     C                   KFLD                    v1cAAS
237200990622     C                   KFLD                    v1cLNP
237300990622     C                   KFLD                    v1cNRS
237400990622     C                   KFLD                    v1cNSP
237500990618      *
237600990623     C     kspe1         KLIST
237700990623     C                   KFLD                    v1cAAS
237800990623     C                   KFLD                    v1cLNP
237900990623     C                   KFLD                    v1cNRS
238000990623     C                   KFLD                    v1cNSP
238100990623     C                   KFLD                    D50tbl
238200990813
238300990813     C     kspe9         KLIST
238400990813     C                   KFLD                    v1cAAS
238500990813     C                   KFLD                    v1cLP1
238600990623      *
238700990622     C     krmn          KLIST
238800990622     C                   KFLD                    v1cRMN
238900990622     C                   KFLD                    v1cCCM
239000990622      *
239100990707     C     kfat1         KLIST
239200990707     C                   KFLD                    v1cKSC
239300990707      *
239400990707     C     kfat2         KLIST
239500990622     C                   KFLD                    v1cKSC
239600990716     C                   KFLD                    Wv1cDFT
239700990622      *
239800990707     C     kfat3         KLIST
239900990622     C                   KFLD                    v1cKSC
240000990716     C                   KFLD                    Wv1cDFT
240100990622     C                   KFLD                    v1cNFT
240200060608     C     kfat4         KLIST
240300060608     C                   KFLD                    v1cfiv
240400060608     C                   KFLD                    v1cNFT
240500060608     C     kfat43        KLIST
240600060608     C                   KFLD                    v1cfiv
240700060608     C                   KFLD                    v1cNFT
240800060608     C                   KFLD                    Wv1cDFT
240900990622      *
241000990622     C     kdta          KLIST
241100010112     C                   KFLD                    klinea
241200000717     C                   KFLD                    KAAS
241300990622     C                   KFLD                    KMGS
241400000718      *
241500000718     C     kdta1         KLIST
241600010112     C                   KFLD                    klinea
241700000718     C                   KFLD                    KAAS
241800000718     C                   KFLD                    KMGS
241900000718     C                   KFLD                    WorkSerie
242000000718      *
242100000718     C     kdta2         KLIST
242200010112     C                   KFLD                    klinea
242300000718     C                   KFLD                    KAAS
242400000718     C                   KFLD                    KMGS
242500000718     C                   KFLD                    WorkSerie
242600000718     C                   KFLD                    KSEGNA
242700000223      *
242800000223     C     ktas          KLIST
242900000223     C                   KFLD                    ta4AAS
243000000223     C                   KFLD                    ta4LNP
243100000223     C                   KFLD                    ta4NRS
243200000223     C                   KFLD                    ta4NSP
243300990622      *
243400990629     C     ktaa          KLIST
243500990622     C                   KFLD                    tasAAS
243600990622     C                   KFLD                    tasLNP
243700990622     C                   KFLD                    tasNRS
243800990622     C                   KFLD                    tasNSP
243900990629     C                   KFLD                    Ktrc
244000990623      *
244100990623     C     ktat          KLIST
244200010112     C                   KFLD                    v1cfls
244300010112     C                   KFLD                    v1cnrr
244400010112     C                   KFLD                    v1cnsc
244500010112     C                   KFLD                    v1clna
244600000717      *
244700000717     C     ktat1         KLIST
244800010112     C                   KFLD                    v1cfls
244900010112     C                   KFLD                    v1cnrr
245000010112     C                   KFLD                    v1cnsc
245100000717      *
245200010112     C     ktats         KLIST
245300000717     C                   KFLD                    TATAAS
245400000717     C                   KFLD                    TATLNP
245500000717     C                   KFLD                    TATNRS
245600000717     C                   KFLD                    TATNSP
245700000717      *
245800000717     C     kspe11        KLIST
245900000717     C                   KFLD                    v1cLP1
246000000717     C                   KFLD                    v1cRSD
246100000717      *
246200000717     C     kspe12        KLIST
246300000717     C                   KFLD                    kAAS
246400000717     C                   KFLD                    v1cLP1
246500000717     C                   KFLD                    WorkSerie
246600000901      *
246700000901     C     ktah          KLIST
246800000901     C                   KFLD                    KKKTRC            1
246900000901     C                   KFLD                    V1Cnot
247000000901      *
247100000911     C                   MOVE      'C'           KKKTRC                         "Chi Sono"
247200000901      *
247300000901     C     ktahxs        KLIST
247400000901     C                   KFLD                    TAHAAS
247500000901     C                   KFLD                    TAHLNP
247600000901     C                   KFLD                    TAHNRS
247700000901     C                   KFLD                    TAHNSP
247800030529     C*
247900030529     C* Chiave su TNTBE01L - Parziale
248000030529     C     KTBE01P       KLIST
248100030529     C                   KFLD                    TBECOD
248200030529     C                   KFLD                    TBEKE1
248300040216      *
248400040216      * Chiave su TABEL00F
248500040216     c     K03TAB00      klist
248600040216     c                   kfld                    TBLkut
248700040216     c                   kfld                    TBLcod
248800040216     c                   kfld                    TBLkey
248900990623
249000990618     C                   ENDSR
249100990618      *---------------------------------------------------------------------------------------------
249200990618** MSG  Lungh. 78                                                            *
249300120517TNSB50R- Livello procedura chiamante non valida. Avvertire il C.E.D.            1 - Not Used
249400990623TNSB50R- Codice cliente errato                                                  2
249500990623TNSB50R- Non richiedere il cod. mittente se non si è immesso il rif. mittente   3
249600990623TNSB50R- Impostate selezioni incongruenti                                       4
249700990623TNSB50R- Data fattura errata                                                    5
249800990623TNSB50R- Immettere la linea                                                     6
249900990623TNSB50R- Linea inesistente o annullata                                          7
250000120517Non richiedere data, numero fattura o P.O.IVA e le spedizioni NON fatturate     8
250100990623TNSB50R- Immettere l'anno spedizione                                            9
250200120517Se immesso numero FATTURA richiedere anche cod.Cliente Fatturazione o P.O. IVA  10
250300120517Se richiesta Data Fattura immettere Cod.fatturazione o P.O IVA e NumeroFattura  11
250400990623TNSB50R- Non richiedere sped. non fatturate se non è immesso il cod. fatturaz.  12
250500990623TNSB50R- Data spedizione errata                                                 13
250600990623TNSB50R- Immettere data spedizione DAL minore o uguale a data spedizione AL     14
250700990623TNSB50R- Immettere almeno una selezione valida                                  15
250800990623TNSB50R- Non richiedere la serie se non immessi anche P.O. e numero segnacollo  16
250900990623TNSB50R- Non richiedere il P.O. segnacollo se non immesso il numero segnacollo  17
251000990623TNSB50R- Non richiedere il numero segnacollo se non immesso il P.O. segnacollo  18
251100990623TNSB50R- Bolla esistente ma annullata                                           19
251200990623TNSB50R- Opzione non valida                                                     20
251300990623TNSB50R- Scorrimento indietro o in avanti oltre il primo o l'ultimo record      21
251400120517TNSB50R- Non trovata la spedizione richiesta                                    22-Not Used
251500990726TNSB50R- Immettere il numero spedizione                                         23
251600000714TNSB50R- Immettere un valore numerico                                           24
251700000714TNSB50R- Immettere anche il P.O. Partenza                                       25
251800010129TNSB50R- Non richiedere la serie, altre serie inserite                          26
251900000717TNSB50R- Immettere anche la data di spedizione                                  27
252000120517TNSB50R- Non richiedere la linea se non immessi anche il numero segnacollo      28-Not Used
252100000718TNSB50R- Immettere al massimo il mese successivo della Data Spedizione dal      29
252200120517TNSB50R- Immettere anche il P.O. Arrivo- (serie è > di 0)                       30-Not Used
252300010219TNSB50R- Immettere il numero ORM                                                31
252400010219TNSB50R- Immettere il p.o. emissione                                            32
252500010219TNSB50R- P.O. emissione inesistente o annullato                                 33
252600010219TNSB50R- Non richiedere serie o viaggio se non inserito il n.orm                34
252700040216TNSB50R- Codice bolla errato                                                    35
252800060609Se immesso P.O. IVA immettere anche il Numero Fattura                           36
252900060609Se immesso P.O. IVA non immettere il cod.cliente fatturazione e viceversa       37
253000060609Se immesso cod.Cliente Fatturazione e numero fattura immettere anche la DATA    38
253100060614Parcel DPD formalmente errato: o lungo 11 numerico o lungo 14 alfanumerico      39
253200120517Caricato il numero massimo di spedizioni a video, anche se ne sono altre...!    40
253300060609**
2534000606090123456789
