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
002100121025      * 37 -
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
017500121025     D*wrkoggiiso      s               d
017600121025     D*wDataLimitePT   s              8  0
017700121025     D*wGGLimPT        s              4  0
017800990618      *
017900990618      *   S C H I E R E
018000120517     D MSG             S             78    DIM(40) CTDATA PERRCD(1)             MSG VIDEO
018100120517     D NUM             S              1    DIM(10) CTDATA PERRCD(10)
018200000223     D SPED            S             16  0 DIM(90)
018300000223     D FILDPD          S              3s 0 DIM(20)
018400121025     D*FILPOSTE        S              3s 0 DIM(20)
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
024000121025     D*DVPO          E DS
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
030200121025     C********           eval      v1csnp = 'N'
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
032200120802     c                   eval      $Full = *off
032300990623      *
032400990618      * f3=Fine
032500990618     c   kc              goto      fine
032600990623      *
032700990618      * Controlli 1a videata
032800990618     c                   exsr      ctrd01
032900990623      *
033000990622     c                   IF        *IN28 = *ON  or  *IN90 = *ON
033100990622     c                   goto      for01
033200990622     c                   ENDIF
033300990623      *
033400990623     c                   ENDIF
033500990623
033600990618      * Carico subfile
033700990618     C                   eval      *in23 = *off
033800990618     C                   clear                   nrr1
033900990623     c                   clear                   NUMspedcar
034000000223     c                   clear                   sped
034100061120     c                   clear                   wgiro
034200990618      *
034300990624     C                   exsr      RIEd02
034400990729      *
034500990729      * EMISSIONE SUBFILE O ENTRATA DIRETTA NELLA SPEDIZIONE
034600990624     c                   SELECT
034700990708      *
034800990729      * Trovati 1 + n record validi: emetto il subfile.
034900990706     C                   WHEN       NRR1 > 1
035000990729      *
035100990729      * Trovato 1 record valido. Interrogazione per selezione emetto il subfile
035200990729     C                   WHEN       NRR1 = 1  and  %SUBST(I50OP0:2:2) = '01'
035300990708      *
035400990729      * Trovato 1 record valido. Gestione della spedizione, se torna con errore emetto il subfile
035500990729     C                   WHEN       NRR1 = 1  and  %SUBST(I50OP0:2:2) = '02'
035600990729     c                   exsr      SUB_opz02
035700990729     c                   If        D01msg <> *blanks
035800990729     c                   movel     D01msg        V1Cmsg
035900990729     c                   eval      *in90 = *on
036000990730     c                   eval      *in28 = *on
036100990729     c                   Else
036200990729     c                   clear                   SB50D01
036300990729     c                   move      wdtgio        v1caas
036400121025     c****               eval      v1csnp = 'N'
036500990729     c                   clear                   WV1Cccm
036600990729     c                   clear                   WV1Cksc
036700060608     c                   eval      v1dfiv=savfiv
036800010112     c                   goto      FOR01
036900990729     c                   Endif
037000990729      * Trovato 1 record valido. Interrogazione, visualizzo la spedizione.
037100990729      *  Se chiamato con opz 05 torno al chiamante
037200990624     C                   WHEN       NRR1 = 1
037300990624     c                   exsr      SUB_opz05
037400990706     c                   clear                   SB50D01
037500990706     c                   move      wdtgio        v1caas
037600121025     c****               eval      v1csnp = 'N'
037700990707     c                   clear                   WV1Cccm
037800990707     c                   clear                   WV1Cksc
037900060608     c                   eval      v1dfiv=savfiv
038000990708     c                   If        %SUBST(I50OP0:2:2) = '05'
038100990708     c                   goto      FINE
038200990708     c                   Else
038300041008      * se richiamato da reclami e selezionato la bolla vado a fine
038400041008if  1c                   if            SDSprm > 1
038500041008     c                             and IA1ins = 'S'
038600041008     c                   goto      FINE
038700041008e   1c                   endif
038800041008      *
038900010112     c                   goto      FOR01
039000990708     c                   Endif
039100990708      *
039200990708      * Trovati 0 record validi e chiamato con opzione 05: ritorno al chiamante una segnalazione.
039300990729     c                   WHEN       NRR1 = *zeros and %SUBST(I50OP0:2:2) = '05'
039400990624     c                   eval      O50err = '1'
039500990726     c                   movel     msg(22)       O50msg
039600990624     c                   movel     TNSB50DS      KPJBU
039700990624     c                   goto      FINE
039800990708      *
039900990708      * Trovati 0 record validi e NON chiamato con opzione 05: emetto segnalazione e torno a VIDEO 1
040000990729     c                   WHEN       NRR1 = *zeros
040100990624     C     *INKL         DOWEQ     *OFF
040200990624     c                   write     SB50T01
040300990624     C                   exfmt     SB50D03
040400990624     C                   ENDDO
040500010112     c                   goto      FOR01
040600990624      *
040700990624     C                   ENDSL
040800990618
040900990618      * Emissione VIDEO2
041000990618     C     for02         tag
041100990618     C                   exsr      EMISd02
041200990618
041300990618      * F03 = Fine
041400990618     c   kc              goto      fine
041500990618
041600990708      * F12 = Precedente - Se chiamato con opzione "05" torno al chiamante
041700990707     c                   IF        *inKL = *ON
041800990708     c                   If        %SUBST(I50OP0:2:2) = '05'
041900990708     c                   goto      FINE
042000990708     c                   Else
042100990707     c                   clear                   SB50D01
042200990707     c                   move      wdtgio        v1caas
042300121025     c*****              eval      v1csnp = 'N'
042400990707     c                   clear                   WV1Cccm
042500990707     c                   clear                   WV1Cksc
042600060608     c                   eval      v1dfiv=savfiv
042700010112     c                   goto      for01
042800990708     c                   Endif
042900990707     c                   ENDIF
043000990618
043100990618      * ROLLUP = Carico il subfile
043200990623     c                   IF        *IN23 = *ON
043300990618     c                   exsr      Ried02
043400990623     c                   If        Wspedpag = *zeros
043500990623     c                   movel     msg(21)       V1Cmsg
043600990618     c                   eval      *in28 = *on
043700990618     c                   Endif
043800990618     c                   GOTO      for02
043900990618     c                   ENDIF
044000990618
044100990618      * Controlli 2a videata
044200990618     c                   exsr      ctrd02
044300990618
044400990624      * Se selezionata una spedizione (Opz. 1) vado a fine altrimenti riemetto video 2
044500990624     c                   IF        Wselsped = '1'
044600990624     c                   movel     TNSB50DS      KPJBU
044700990624     c                   ELSE
044800990618     c                   goto      for02
044900990624     C                   ENDIF
045000990618
045100990618     c     fine          tag
045200990618
045300990618      * Chiudo pgm aperti
045400990618     C                   IF        Wtibs69r  <>  *BLANKS
045500990618     C                   eval      I69TLA  = 'C'
045600990618     C                   call      'TIBS69R'
045700990618     C                   parm                    TIBS69DS
045800990624     c                   ENDIF
045900990618
046000990624      * Imposto campi per uscita con F03
046100990624     c                   IF        *INKC = *ON
046200990624     c                   movel     TNSB50DS      W006A
046300990624     c                   clear                   TNSB50DS
046400990624     c                   movel     W006A         TNSB50DS
046500990624     c                   movel     '1'           O50F03
046600040311     c                   movel     TNSB50DS      KPJBU
046700990624     c                   ENDIF
046800990624
046900990618     c                   EVAL      *INLR = *ON
047000990618      **********************************************************************
047100060608      * Impostazione campi della prima videata
047200990618      **********************************************************************
047300060608     C     IMPO          BEGSR
047400060608     c* Chain tabella QC recod 2 per avere i libri iva
047500060608     c                   movel     CodUt         TBLkut
047600060608     c                   movel     'QC'          TBLcod
047700060608     c                   movel(p)  '2'           TBLkey
047800060608     c     K03TAB00      chain     TABEL
047900060608     c                   if        %found(tabel00f)
048000060608     c                   movel     tbluni        dsqc2
048100060608     c                   eval      v1dfiv=fiv_cost
048200060608     c                   eval      %subst(v1dfiv:1:3)=(%editc(§qcfiv: 'X'))
048300060608     c                   eval      %subst(v1dfiv:15:3)=(%editc(§qcfi2: 'X'))
048400060608     c                   endif
048500060608     c* salvo la stringa
048600060608     c                   movel     v1dfiv        savfiv
048700060608     c                   ENDSR
048800060608      **********************************************************************
048900060608      * CONTROLLI VIDEO 1
049000060608      **********************************************************************
049100060608     C     Ctrd01        BEGSR
049200990618
049300990621     C                   clear                   wselez
049400990621
049500990623      *  NUMERO SPEDIZIONE
049600990623     c                   IF        V1Cnsp <> *zeros  or  V1Clnp <> *zeros
049700990623      * Numero
049800990618     c                   IF        V1Cnsp = *zeros
049900990618     c                   eval        *IN40 = *on
050000990618     c                   eval        *IN28 = *on
050100990618     c                   eval        *IN90 = *on
050200990726     c                   movel     MSG(23)       V1Cmsg
050300990618     c                   ELSE
050400990623      * P.O
050500990623     C                   z-add     V1Clnp        Wlinea
050600990618     c                   exsr      CTR_linea
050700990618     c                   IF        *IN28 = *ON
050800990618     c                   eval        *IN40 = *on
050900990618     C                   ELSE
051000990623      * Anno
051100990623     c                   z-add     V1Caas        Wanno
051200990618     c                   exsr      CTR_anno
051300990618     c                   IF        *IN28 = *ON
051400990618     c                   eval        *IN41 = *on
051500990618     c                   else
051600990623     c                   z-add     Wanno         V1Caas
051700990618     c                   ENDIF
051800990618      *
051900990618     c                   ENDIF
052000990621     c                   ENDIF
052100990623     c                   ENDIF
052200990621      *
052300990621      * Controllo selezione
052400990621     c                   SELECT
052500990621     c                   WHEN      *IN28 = *ON
052600990621     c                   WHEN      V1Cnsp > *zeros
052700990621     c                   z-add     1             Wselez
052800990621     c                   ENDSL
052900990621      *
053000990621     c   28              goto      EndCTRD01
053100010219
053200010219      *  NUMERO ORM
053300010219     c                   IF        ds_orm <> *zeros
053400010219
053500010219      * obbligatorio il numero orm se inserito p.o.emissione
053600010219     C                   if        v1cpoe <> *zeros and v1cnor = *zeros
053700010219     c                   eval        *IN54 = *on
053800010219     c                   eval        *IN28 = *on
053900010219     c                   movel     MSG(31)       V1Cmsg
054000010219     C                   goto      endctrd01
054100010219     C                   endif
054200010219
054300010219      * obbligatorio il p.o. emissione se inserito il numero orm
054400010219     c                   if        v1Cpoe = *zeros  and  v1cnor <> *zeros
054500010219     c                   eval        *IN55 = *on
054600010219     c                   eval        *IN28 = *on
054700010219     c                   movel     MSG(32)       V1Cmsg
054800010219     C                   goto      endctrd01
054900010219     C                   endif
055000010219
055100010219      * già impostata una selezione
055200010219     c                   if        Wselez > *zeros
055300010219     c                   movel     MSG(4)        V1Cmsg
055400010219     c                   eval      *IN55 = *on
055500010219     c                   eval      *IN28 = *on
055600010219     C                   goto      endctrd01
055700010219     C                   endif
055800010219
055900010219      * non si può inserire la serie senza il p.o.e numero orm
056000010219     c                   if        V1Cnsr > *zeros  and
056100010219     c                             V1Cpoe = *zeros  and V1Cnor = *zeros
056200010219     c                   eval        *IN54 = *on
056300010219     c                   eval        *IN28 = *on
056400010219     c                   movel     MSG(34)       V1Cmsg
056500010219     C                   goto      endctrd01
056600010219     C                   endif
056700010219
056800010219      * non si può inserire il viaggio senza il p.o.e numero orm
056900010219     c                   if        V1Cnrv > *zeros  and
057000010219     c                             V1Cpoe = *zeros  and V1Cnor = *zeros
057100010219     c                   eval        *IN54 = *on
057200010219     c                   eval        *IN28 = *on
057300010219     c                   movel     MSG(34)       V1Cmsg
057400010219     C                   goto      endctrd01
057500010219     C                   endif
057600010219
057700010219      * Controlla il p.o.emissione
057800010219     C                   z-add     V1cpoe        Wlinea
057900010219     c                   exsr      CTR_linea
058000010219     c                   eval      *IN55 = (*in28 = *on)
058100010219     c   28              movel     MSG(33)       V1Cmsg
058200010219     C   28              goto      endctrd01
058300010219
058400010219      * imposta la selezione da fare
058500010219     C                   z-add     15            Wselez
058600010219     c                   ENDif
058700010112
058800010112      * SEGNACOLLO
058900010112     C                   if        v1cfls <> *zeros or v1cnsc <> *zeros
059000010112
059100010112      * obbligatorio il numero segnacollo se inserito p.o.segnacollo
059200010112     C                   if        v1cfls <> *zeros and v1cnsc =  *zeros
059300010112     c                   eval        *IN51 = *on
059400010112     c                   eval        *IN28 = *on
059500010112     c                   movel     MSG(17)       V1Cmsg
059600010112     C                   goto      endctrd01
059700010112     C                   endif
059800010112
059900010112      * obbligatorio il p.o. segnacollo se inserito il numero segnacollo
060000010112     c                   if        v1Cfls = *zeros  and  v1cnsc <> *zeros
060100010112     c                   eval        *IN51 = *on
060200010112     c                   eval        *IN28 = *on
060300010112     c                   movel     MSG(18)       V1Cmsg
060400010112     C                   goto      endctrd01
060500010112     C                   endif
060600010112
060700010112      * già impostata una selezione
060800010112     c                   if        Wselez > *zeros
060900010112     c                   movel     MSG(4)        V1Cmsg
061000010112     c                   eval      *IN51 = *on
061100010112     c                   eval      *IN28 = *on
061200010112     C                   goto      endctrd01
061300010112     C                   endif
061400010112
061500010112      * Controlla il p.o.segnacollo
061600010112     C                   z-add     V1cfls        Wlinea
061700010112     c                   exsr      CTR_linea
061800010112     c                   eval      *IN51 = (*in28 = *on)
061900010112     c   28              movel     MSG(7)        V1Cmsg
062000010112     C   28              goto      endctrd01
062100010112
062200010112      * imposta la selezione da fare
062300010112     C                   z-add     13            Wselez
062400010112     c                   ENDif
062500010112
062600010112      * non si può inserire la serie senza il p.o.e numero segnacollo
062700010112     c                   if        V1Cnrr > *zeros  and
062800010112     c                             V1Cfls = *zeros  and V1Cnsc = *zeros
062900010112     c                   eval        *IN51 = *on
063000010112     c                   eval        *IN28 = *on
063100010112     c                   movel     MSG(16)       V1Cmsg
063200010112     C                   goto      endctrd01
063300010112     C                   endif
063400990618
063500000901      * CODICE "CHI SONO"
063600000901     C     V1Cnot        IFNE      *blanks
063700000901     C     Wselez        IFEQ      *zeros
063800000901     C                   Z-ADD     14            Wselez
063900000901     C                   ELSE
064000000901     c                   movel     MSG(4)        V1Cmsg
064100000901     c                   eval      *IN40 = *on
064200000901     c                   eval      *IN28 = *on
064300000901     C                   ENDIF
064400000901     C                   ENDIF
064500000901      *
064600000901     c   28              goto      EndCTRD01
064700000901
064800040216      * RIFERIMENTO NUMERICO MITTENTE e CODICE CLIENTE MITTENTE
064900990618     c                   SELECT
065000990618      *    se immessa rag. sociale e non il codice mittente richiamo il pgm di ricerca alfabetica
065100990618     c                   WHEN      V1Cccm = *zeros  and  V1Dccm <> *blanks
065200990618     c                   movel(P)  V1Dccm        PAXdmt
065300990618     c                   exsr      ricalf
065400990618     C                   If        PAXsta > -1
065500000901     C                   movel     paxksm        V1Cccm
065600990621     c                   movel(P)  PAXdmt        V1Dccm
065700990618     C                   Endif
065800990618     c                   eval      *IN90 = *on
065900990621      *    se immesso il codice mittente lo decodifico
066000990618     c                   WHEN      V1Cccm > *zeros  and  V1Cccm <> WV1Cccm
066100990621     c                   clear                   V1Dccm
066200990621     c                   move      V1Cccm        w0040
066300990621     c                   IF        W0040 <> 9999  and  W0040 <> 8888
066400990621     c                   z-add     V1Cccm        I69kac
066500990621     c                   exsr      CTR_anagra
066600990707     C                   If         WO69ERR =  '1'
066700990623     c                   movel     MSG(2)        V1Cmsg
066800990621     c                   eval      *IN43 = *on
066900990621     c                   eval      *IN28 = *on
067000990621     c                   Else
067100990621     c                   eval      *IN90 = *on
067200990621     c                   movel     ACOrag        V1Dccm
067300990621     c                   Endif
067400990621     c                   ENDIF
067500990618      *    se inserito il codice mittente e non il riferimento mittente errore
067600990623     c                   WHEN      V1Cccm > *zeros  and  V1Crmn = *zeros
067700990623     c                   movel     MSG(3)        V1Cmsg
067800990621     c                   eval      *IN43 = *on
067900990621     c                   eval      *IN28 = *on
068000990621      *
068100990621     c                   ENDSL
068200990621      *
068300990621      * Controllo selezione
068400990621     c                   SELECT
068500990621     c                   WHEN      V1Crmn = *zeros
068600990621     c                   WHEN      *IN28 = *ON
068700990621     c                   WHEN      Wselez > *zeros
068800990623     c                   movel     MSG(4)        V1Cmsg
068900990621     c                   eval      *IN42 = *on
069000990621     c                   eval      *IN28 = *on
069100990621     c                   WHEN      V1Cccm > *zeros
069200990621     c                   z-add     3             Wselez
069300990621     c                   OTHER
069400990621     c                   z-add     2             Wselez
069500990621     c                   ENDSL
069600990621      *
069700990623     c                   z-add     V1Cccm        WV1Cccm
069800990623      *
069900990621     c   28              goto      EndCTRD01
070000040216
070100040216      * RIFERIMENTO ALFABETICO MITTENTE
070200040216      *   Controllo selezione
070300040216     c                   SELECT
070400040216     c                   WHEN      V1Crma = *blanks
070500040216     c                   WHEN      Wselez > *zeros
070600040216     c                   movel     MSG(4)        V1Cmsg
070700040216     c                   eval      *IN50 = *on
070800040216     c                   eval      *IN28 = *on
070900040216     c                   OTHER
071000040216     c                   z-add     16            Wselez
071100040216     c                   ENDSL
071200040216     c   28              goto      EndCTRD01
071300990618
071400990621      * RIFERIMENTO PARTNER
071500990621      *   Controllo selezione
071600990621     c                   SELECT
071700990623     c                   WHEN      V1Crpt = *blanks
071800990621     c                   WHEN      Wselez > *zeros
071900990623     c                   movel     MSG(4)        V1Cmsg
072000990621     c                   eval      *IN44 = *on
072100990621     c                   eval      *IN28 = *on
072200990621     c                   OTHER
072300990621     c                   z-add     4             Wselez
072400990621     c                   ENDSL
072500990621     c   28              goto      EndCTRD01
072600000211
072700000211      * NUMERO PARCEL DPD
072800060608     c* deve essere lungo o 11 o 14
072900060609     c                   select
073000060609     c* OK
073100060609     c                   when      v1cdpd=*blanks
073200060609     c                   when      %subst(v1cdpd:14:1)<>' '
073300060609     c* OK
073400060609     c                   when      %subst(v1cdpd:12:3)='   '  and
073500060609     c                             %subst(v1cdpd:11:1)<>' '
073600060609     c
073700060609     c* Se lungo 11 deve essere tutto numerico
073800060609     c                   movea     v1cdpd        w11
073900060609     c                   do        11            xx                3 0
074000060609     c     w11(XX)       lookup    num                                    30
074100060609     c                   if        not *in30
074200060609     c                   eval      *IN52 = *on
074300060609     c                   eval      *IN28 = *on
074400060609     c                   movel     MSG(39)       V1Cmsg
074500060609     c                   leave
074600060609     c                   endif
074700060609     c
074800060609     c                   enddo
074900060609     c
075000060609     c                   other
075100060609     c* parcel dpd errato
075200060609     c                   eval      *IN52 = *on
075300060609     c                   eval      *IN28 = *on
075400060609     c                   movel     MSG(39)       V1Cmsg
075500060609     c                   endsl
075600060609     c   28              goto      EndCTRD01
075700060609     c
075800000211      *   Controllo selezione
075900000211     c                   SELECT
076000060608     c                   WHEN      V1Cdpd = *blanks
076100000211     c                   WHEN      Wselez > *zeros
076200000211     c                   movel     MSG(4)        V1Cmsg
076300000211     c                   eval      *IN52 = *on
076400000211     c                   eval      *IN28 = *on
076500000211     c                   OTHER
076600000211     c                   z-add     10            Wselez
076700000211     c                   ENDSL
076800000211     c   28              goto      EndCTRD01
076900990621
077000990621      * CODICE CLIENTE FATTURAZIONE e DATA FATTURA e NUMERO FATTURA
077100060608     c*  P.O. IVA
077200990621      * Controllo data fattura
077300990621     C                   clear                   WV1Cdft
077400990621     C                   IF        V1Cdft  > *zeros
077500990805     C                   z-add     V1Cdft        WDATA_08
077600990805     C                   exsr      CTR_data
077700990805     C                   IF        *in28 = *on
077800990805     C                   movel     MSG(5)        V1CMSG
077900990805     c                   eval      *in46 = *ON
078000990805     C                   clear                   WV1Cdft
078100990805     C                   ELSE
078200990805     C                   z-add     Wdata_08g     WV1Cdft
078300990805     C                   z-add     Wdata_08      V1Cdft
078400990621     C                   ENDIF
078500990805     C                   ENDIF
078600990805      *
078700990805     c   28              goto      EndCTRD01
078800990621      *
078900990621     C                   SELECT
079000990621      *    se immessa rag. sociale e non il codice cliente richiamo il pgm di ricerca alfabetica
079100990621     c                   WHEN      V1Cksc = *zeros  and  V1Dksc <> *blanks
079200990621     c                   movel(P)  V1Dksc        PAXdmt
079300990621     c                   exsr      ricalf
079400990621     C                   If        PAXsta > -1
079500000901     C                   movel     paxksm        V1Cksc
079600990621     c                   movel(P)  PAXdmt        V1Dksc
079700990621     C                   Endif
079800990621     c                   eval      *IN90 = *on
079900990621      *    se immesso il codice cliente fattura lo decodifico
080000990621     c                   WHEN      V1Cksc > *zeros  and  V1Cksc <> WV1Cksc
080100990621     c                   clear                   V1Dksc
080200990621     c                   move      V1Cksc        w0040
080300990621     c                   IF        W0040 <> 9999  and  W0040 <> 8888
080400990621     c                   z-add     V1Cksc        I69kac
080500990621     c                   exsr      CTR_anagra
080600990707     C                   If         WO69ERR =  '1'
080700990623     c                   movel     MSG(2)        V1Cmsg
080800990621     c                   eval      *IN45 = *on
080900990621     c                   eval      *IN28 = *on
081000990621     c                   Else
081100990621     c                   eval      *IN90 = *on
081200990621     c                   movel     ACOrag        V1Dksc
081300990621     c                   Endif
081400990621     c                   ENDIF
081500060608      *    Se immesso codice cliente non immettere p.o. iva e viceversa     codice fatturazione
081600060608     c                   WHEN      V1Cfiv>0 and v1cksc>0
081700060608     c                   movel     MSG(37)       V1Cmsg
081800060608     c                   eval      *IN45 = *on
081900060608     c                   eval      *IN28 = *on
082000990623      *    non è possibile indicare la data fattura e richiedere le spedizioni non fatturate
082100060608     c                   WHEN      V1Cnfa <> *blanks  and
082200060608     c                             (v1cdft>0 or v1cfiv>0 or v1cnft>0)
082300990623     c                   movel     MSG(8)        V1Cmsg
082400990623     c                   eval      *IN46 = *on
082500990623     c                   eval      *IN28 = *on
082600060608      *    non è possibile indicare il numero fattura senza il codice cliente
082700060608      *    o P.O. iva
082800060608     c                   WHEN      V1Cksc = *zeros  and  V1Cnft > *zeros
082900060608     c                             and v1cfiv=0
083000990623     c                   movel     MSG(10)       V1Cmsg
083100990623     c                   eval      *IN46 = *on
083200990623     c                   eval      *IN28 = *on
083300060608      *    se immesso cliente e numero fattura immettere anche la data      e
083400060608     c                   WHEN      V1Cksc>0  and  V1Cnft > *zero
083500060608     c                             and v1cdft=0
083600060608     c                   movel     MSG(38)       V1Cmsg
083700060608     c                   eval      *IN46 = *on
083800060608     c                   eval      *IN28 = *on
083900990623      *    non è possibile indicare la data fattura senza il codice fatturazione
084000060608     c                   WHEN      v1Cksc = *zeros  and  V1Cdft > *zeros
084100060608     c                             and v1cnft=0 and v1cfiv=0
084200990623     c                   movel     MSG(11)       V1Cmsg
084300990621     c                   eval      *IN45 = *on
084400990621     c                   eval      *IN28 = *on
084500060608      *    Se immesso p.o. IVA immettere anche numero fattura               codice fatturazione
084600060608     c                   WHEN      V1Cfiv>0 and v1cnft=0
084700060608     c                   movel     MSG(36)       V1Cmsg
084800060608     c                   eval      *IN57 = *on
084900060608     c                   eval      *IN28 = *on
085000990623      *    non è possibile selezionare le spedizioni non fatturate senza il codice fatturazione
085100990623     c                   WHEN      V1Cksc = *zeros  and  V1Cnfa <> *blanks
085200990623     c                   movel     MSG(12)       V1Cmsg
085300990623     c                   eval      *IN45 = *on
085400990623     c                   eval      *IN28 = *on
085500990621      *
085600990621     c                   ENDSL
085700990621      *
085800990621      * Controllo selezione
085900990621     c                   SELECT
086000060608     c                   WHEN      V1Cksc = *zeros  and v1cfiv=0
086100990621     c                   WHEN      *IN28 = *ON
086200990621     c                   WHEN      Wselez > *zeros
086300990623     c                   movel     MSG(4)        V1Cmsg
086400990621     c                   eval      *IN45 = *on
086500990621     c                   eval      *IN28 = *on
086600060608     c                   WHEN      v1cfiv>0
086700060608     c                   z-add     17            Wselez
086800060608     c                   WHEN      v1cksc>0 and V1Cnft > *zeros
086900060608     c                   z-add     7             Wselez
087000060608     c                   WHEN      V1Cdft > *zeros  or  V1Cnfa <> *blanks
087100990621     c                   z-add     6             Wselez
087200990621     c                   OTHER
087300990621     c                   z-add     5             Wselez
087400990621     c                   ENDSL
087500990623      *
087600990623     c                   z-add     V1Cksc        WV1Cksc
087700990623      *
087800990621     c   28              goto      EndCTRD01
087900990618
088000990621      * DATA SPEDIZIONE DAL/AL
088100990621     C                   clear                   WV1Cdsd
088200990622     C                   clear                   WV1Cdsa
088300990623      *     spedizione al
088400990623     C                   IF        V1Cdsa  > *zeros
088500990805     C                   z-add     V1Cdsa        WDATA_08
088600990805     C                   exsr      CTR_data
088700990805     C                   IF        *in28 = *on
088800990805     c                   eval      *in48 = *ON
088900990805     C                   ELSE
089000990805     C                   z-add     Wdata_08g     WV1Cdsa
089100990805     C                   z-add     Wdata_08      V1Cdsa
089200990805     C                   ENDIF
089300990623     C                   ENDIF
089400990623      *     spedizione dal
089500110110     C                   IF        V1Cdsd  > *zeros and not *in28
089600990805     C                   z-add     V1Cdsd        WDATA_08
089700990805     C                   exsr      CTR_data
089800990805     C                   IF        *in28 = *on
089900990805     c                   eval      *IN47 = *on
090000990805     c                   eval      *IN48 = *off
090100990805     C                   ELSE
090200990805     C                   z-add     Wdata_08g     WV1Cdsd
090300990805     C                   z-add     Wdata_08      V1Cdsd
090400990805     C                   ENDIF
090500990621     C                   ENDIF
090600990622      *
090700990622      * Gestione errori, controllo congruità date e Controllo selezione
090800990621     c                   SELECT
090900990623     c                   WHEN      *IN47 = *ON  or  *IN48 = *ON
091000990623     c                   movel     MSG(13)       V1Cmsg
091100990623     c                   eval      *IN28 = *on
091200990622      *
091300990624     c                   WHEN      WV1Cdsa < WV1Cdsd  and  WV1Cdsa <> *zeros
091400990623     c                   movel     MSG(14)       V1Cmsg
091500990621     c                   eval      *IN28 = *on
091600990621     c                   eval      *IN47 = *on
091700010112      **!!!****
091800010112      *   obbligatoria la data se selezione 13 e segnacollo non univoco
091900010112     c                   WHEN      wselez = 13 and  V1flgs = *blanks
092000010112     C                             and v1cfls > *zeros and v1cdsd = *zeros
092100010112     c                   movel     MSG(27)       V1Cmsg
092200010112     c                   eval      *IN28 = *on
092300010112     c                   eval      *IN47 = *on
092400010112     c                   goto      EndCTRD01
092500010112      **!!!****
092600990621      *
092700990621     c                   WHEN      V1Cdsd = *zeros
092800000714      *
092900000714      *              obbligatorio P.O. partenza
093000000714     c                   WHEN      wselez = *zeros  and  V1Clp1 = *zeros
093100000714     c                   movel     MSG(25)       V1Cmsg
093200000714     c                   eval      *IN28 = *on
093300000714     c                   eval      *IN49 = *on
093400010112
093500000714      *              se selezionato il codice fatturazione (sel. 5) vince data spedizione
093600000714      ******** ! >>> se si disasterisca ATTENZIONE xchè è stata aggiunta l'obbligatorietà
093700000714      ******** ! >>> del P.O. di partenza ed è stata cambiata la routine per selezione = 8
093800000714     c******** ! >>>     WHEN      Wselez = 5
093900000714     c******** ! >>>     z-add     8             Wselez
094000000714      *
094100991103     c                   WHEN      Wselez = *zeros
094200991103     c                   z-add     8             Wselez
094300991103      *
094400990813      *
094500990621     c                   ENDSL
094600990622      *
094700990622     C                   If        WV1Cdsa = *zeros
094800990622     c                   z-add     WV1Cdsd       WV1Cdsa
094900990622     C                   Endif
095000990621     c   28              goto      EndCTRD01
095100000718      *
095200000718      * max mese successivo
095300000718     C     Wv1cdsd       ifgt      0
095400000718     C                   movel     Wv1cdsd       WorkAnno          4 0
095500000718     C                   move      Wv1cdsd       Workmesegg        4 0
095600000718     C                   move      '31'          Workmesegg
095700000718     C     Workmesegg    ifeq      1231
095800000718     C                   z-add     131           Workmesegg
095900000718     C                   add       1             Workanno
096000000718     C                   else
096100000718     C                   add       100           Workmesegg
096200000718     C                   endif
096300000718     C                   movel     workanno      workdata          8 0
096400000718     C                   move      workmesegg    workdata
096500000718     C     wv1cdsa       ifgt      workdata
096600000718     c                   movel     MSG(29)       V1Cmsg
096700000718     C                   seton                                        28  48
096800000718     C                   endif
096900000718     C                   endif
097000000718     c   28              goto      EndCTRD01
097100040216
097200040216      * CODICE BOLLA
097300040216if  1c                   if        V1Ccbo <> *blanks
097400040216     c                   movel     CodUt         TBLkut
097500040216     c                   movel     '3A'          TBLcod
097600040216      *
097700040216     c                   eval      *in30  =  *off
097800040216     c     '?'           scan      V1Ccbo                                 30
097900040216if  2c                   if        *in30 = *on
098000040216     c                   call      'X§TABER'
098100040216     c                   parm                    TBLkut
098200040216     c                   parm                    TBLcod
098300040216     c                   parm                    TBLkey
098400040216     c                   parm                    §DES             30
098500040216     c                   movel     TBLkey        V1Ccbo
098600040216     c                   eval      *in90 = *on
098700040216e   2c                   endif
098800040216      *
098900040216     c                   movel(p)  V1Ccbo        TBLkey
099000040216     c     K03TAB00      chain     TABEL
099100040216if  2c                   if        NOT %found(TABEL00F)
099200040216     c                             or  TBLflg <> *blanks
099300040216     c                   movel     Msg(35)       V1Cmsg
099400040216     c                   seton                                        285690
099500040216     c                   goto      EndCtrD01
099600040216e   2c                   endif
099700040216e   1c                   endif
099800990621
099900000714      * DESTINATARIO
100000000714     C     Wselez        ifeq      *zeros
100100000714     C     V1Crsd        andne     *blanks
100200000714     C                   z-add     11            Wselez
100300000714     C     V1Clp1        ifeq      *zeros
100400000714     c                   eval      *IN28 = *on
100500000714     c                   eval      *IN49 = *on
100600000714     c                   movel     MSG(25)       V1Cmsg
100700000714     C                   endif
100800000714     C                   endif
100900000714     c   28              goto      EndCTRD01
101000000714
101100000714      * P.O. PARTENZA
101200990621     c                   clear                   V1Dlp1
101300990621     C                   IF        V1Clp1 > *zeros
101400990623     C                   z-add     V1Clp1        Wlinea
101500990621     c                   exsr      CTR_linea
101600990621     c                   If        *IN28 = *ON
101700990621     c                   eval        *IN49 = *on
101800990623     c                   movel     MSG(7)        V1Cmsg
101900990621     c                   Else
102000990621     C                   movel     ORGDES        V1Dlp1
102100000717     C     Wselez        ifeq      0
102200000717     c                   movel     MSG(27)       V1Cmsg
102300000718     C                   seton                                        47  28
102400000717     C                   endif
102500990621     C                   Endif
102600990621     C                   ENDIF
102700990621     c   28              goto      EndCTRD01
102800000714
102900000714      * SERIE
103000000714     C                   setoff                                       35  36
103100000714     C     v1cser        ifne      '  '
103200010129     C                   if        v1cnrs <> *zeros
103300010129     C                             or v1cnrr <> *zeros
103400010129     C                   movel     MSG(26)       V1Cmsg
103500010129     C                   eval      *IN53 = *on
103600010129     C                   eval      *IN28 = *on
103700010129     C                   goto      EndCTRD01
103800010129     C                   endif
103900000714     C                   movel     v1cser        wserie1           1
104000000714     C                   move      v1cser        wserie2           1
104100000714     C     wserie2       ifeq      ' '
104200000714     C                   eval      wserie2 = wserie1
104300000714     C                   eval      wserie1 = ' '
104400000714     C                   movel     wserie1       v1cser
104500000714     C                   move      wserie2       v1cser
104600000714     C                   endif
104700000714     C     wserie1       ifeq      ' '
104800000714     C                   eval      wserie1 = '0'
104900000714     C                   endif
105000000714      *
105100000714     C     wserie1       ifeq      '0'
105200000714     C                   seton                                        35
105300000714     C                   endif
105400000714     C  N351             do        9             sr                2 0
105500000714     C                   move      sr            wnumalfa          1
105600000714     C     wserie1       ifeq      wnumalfa
105700000714     C                   seton                                        35
105800000714     C                   endif
105900000714     C                   enddo
106000000714      *
106100000714     C     wserie2       ifeq      '0'
106200000714     C                   seton                                        36
106300000714     C                   endif
106400000714     C  N361             do        9             sr
106500000714     C                   move      sr            wnumalfa          1
106600000714     C     wserie2       ifeq      wnumalfa
106700000714     C                   seton                                        36
106800000714     C                   endif
106900000714     C                   enddo
107000000714      *
107100000714     C     *in35         ifeq      *off
107200000714     C     *in36         oreq      *off
107300000714     c                   movel     MSG(24)       V1Cmsg
107400000714     c                   eval      *IN53 = *on
107500000714     c                   eval      *IN28 = *on
107600000717     C                   else
107700000717     C     Wselez        ifeq      8
107800000717     C                   z-add     12            Wselez
107900000717     C                   endif
108000000714     C                   endif
108100000714     C                   endif
108200000714     c   28              goto      EndCTRD01
108300990621
108400990621      * P.O. ARRIVO
108500990621     c                   clear                   V1Dlna
108600990621     C                   IF        V1Clna > *zeros
108700990623     C                   z-add     V1Clna        Wlinea
108800990621     c                   exsr      CTR_linea
108900990621     c                   If        *IN28 = *ON
109000010104     c                   eval        *IN54 = *on
109100990623     c                   movel     MSG(7)        V1Cmsg
109200990621     c                   Else
109300990621     C                   movel     ORGDES        V1Dlna
109400990621     C                   Endif
109500990621     C                   ENDIF
109600990621     c   28              goto      EndCTRD01
109700990813
109800990813      * CONTROLLO SELEZIONE VALIDA
109900990813     c                   IF        Wselez = *zeros
110000990813     c                   movel     MSG(15)       V1Cmsg
110100990813     c                   eval      *IN40 = *on
110200990813     c                   eval      *IN28 = *on
110300990813     c                   ENDIF
110400990813     c   28              goto      EndCTRD01
110500990621
110600990618     c     Endctrd01     ENDSR
110700990618      *****************************************************************
110800990618      * CARICAMENTO SUBFILE VIDEO 2
110900990618      *****************************************************************
111000990618     C     RIED02        BEGSR
111100120517      *
111200120517      * - Già carico il n° max di rec. nel sfl !!!
111300120517     c                   if        $Full
111400120517     c                   eval      Wendpag = 'S'
111500120517     c                   eval      V1Cmsg  = Msg(40)
111600120517     c                   eval      *in28 = *on
111700120517     c                   leavesr
111800120517     c                   endif
111900990618      *
112000990618      * Pulisco subfile
112100990618     c                   IF        *IN23 = *off
112200990618     C                   eval         *IN21 = *OFF
112300990622     C                   WRITE     SB50C02
112400990618     C                   eval         *IN20 = *ON
112500990618     C                   eval         *IN21 = *ON
112600990618     c                   ENDIF
112700990618      *
112800990618     c                   clear                   Wendpag
112900990623     c                   clear                   Wspedpag
113000990623     c                   z-add     NUMspedcar    NRR1
113100990622      *
113200990618     c                   SELECT
113300990622      * spedizione
113400990622     C                   WHEN      Wselez = 1
113500990618     C                   exsr      RIESEL1
113600040216      * riferimento mittente numerico
113700990622     C                   WHEN      Wselez = 2
113800990622     C                   exsr      RIESEL2
113900040216      * riferimento mittente numerico e codice mittente
114000990622     C                   WHEN      Wselez = 3
114100990622     C                   exsr      RIESEL3
114200990622      * riferimento partner
114300990622     C                   WHEN      Wselez = 4
114400990622     C                   exsr      RIESEL4
114500990622      * codice cliente fatturazione e (data fattura o non fatturate)
114600990707     C                   WHEN      Wselez = 5  or  Wselez = 6
114700990707     C                   exsr      RIESEL56
114800990622      * codice cliente fatturazione e data fattura e numero fattura
114900990622     C                   WHEN      Wselez = 7
115000990622     C                   exsr      RIESEL7
115100000717      * data spedizione (e lnp)
115200990622     C                   WHEN      Wselez = 8
115300990622     C                   exsr      RIESEL8
115400990813      * lnp e data spedizione
115500991103     C*                  WHEN      Wselez = 9
115600991103     C*                  exsr      RIESEL9
115700000211      * numero parcel DPD
115800000211     C                   WHEN      Wselez = 10
115900000222     C                   exsr      M1031
116000000222     C                   exsr      RIESEL10
116100000717      * lnp e destinatario
116200000717     C                   WHEN      Wselez = 11
116300000717     C                   exsr      RIESEL11
116400000717      * anno + lnp + serie
116500000717     C                   WHEN      Wselez = 12
116600000717     C                   exsr      RIESEL12
116700000717      * secco per segnacollo su TITAT27C
116800000717     C                   WHEN      Wselez = 13
116900000717     C                   exsr      RIESEL13
117000000901      * codice "CHI SONO"
117100000901     C                   WHEN      Wselez = 14
117200000901     C                   exsr      RIESEL14
117300010219      * numero ORM
117400010219     C                   WHEN      Wselez = 15
117500010219     C                   exsr      RIESEL15
117600040216      * riferimento mittente alfabetico
117700040216     C                   WHEN      Wselez = 16
117800040216     C                   exsr      RIESEL16
117900060608     c
118000060608     c* P.O. iva e numero fattura
118100060608     C                   WHEN      Wselez = 17
118200060608     C                   exsr      RIESEL17
118300990618      *
118400990618     C                   ENDSL
118500990618      *
118600990618     c                   z-add     1             V2Cnrr
118700990623     c                   z-add     NRR1          NUMspedcar
118800990618      *
118900990618     c                   ENDSR
119000990707      *****************************************************************
119100990707      * CARICAMENTO SUBFILE * SELEZIONE 1 - spedizione
119200990707      *****************************************************************
119300990707     C     RIESEL1       BEGSR
119400990707      *
119500990707      * Se il chiamante ha passato la key sped. con il tipo bolla chaino con chiave completa
119600990707     c                   IF        D50tbl <> *blanks
119700990707     c                             and  %SUBST(I50OP0:2:2) <> '05'
119800990707     C     kspe1         chain     TITAS30C                           24
119900990707     C  N24              exsr      RICsbfl
120000990707     C                   ELSE
120100990707      *
120200990707     c                   IF        *IN23 = *OFF
120300990707     C     kspe          chain     TITAS30C                           24
120400990707     c                   ENDIF
120500990707      *
120600990707     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
120700030529     C*
120800030529     C* Inserisco qui i controlli x verificare la data limite x considerare le
120900030529     C* bolle PT (tale controllo nn viene effettuato se l'utente è del gruppo
121000030529     C* EDP o ISP).
121100121025     C****               if        %subst(KPJBU:2:2) <> '05'
121200121025     C****               if        %subst(KNMUS:1:3) = 'EDP'
121300121025     C****               else
121400121025     C**** tasLNP        lookup    FILPOSTE                               96
121500121025     C****               if        *in96
121600121025     C****               if        tasDCM >  0
121700121025     C****               if        tasDCM >= wDataLimitePT
121800121025     C****               else
121900121025     C****               seton                                        24
122000121025     C***                endif
122100121025     C*****              endif
122200121025     C****               endif
122300121025     C*****              endif
122400121025     C****               endif
122500030529     C*
122600030529     C                   if        *in24 = *off
122700990707     C                   exsr      RICsbfl
122800990707     C     kspe          reade     TITAS30C                               24
122900030529     C                   endif
123000990707     C                   ENDDO
123100990707      *
123200990707     c                   ENDIF
123300990707      *
123400990707      *  Se non ho trovato record aggancio il file delle bolle annullate
123500990707     c                   IF        NUMspedcar = *zeros
123600110502     C     kspe          chain     TNBLA01L                           24
123700110502     c                   If        *in24 = *off
123800110502     c                   movel     msg(19)       V1Cmsg
123900110502     c                   Endif
124000990707     c                   ENDIF
124100990707      *
124200990707     c                   ENDSR
124300990707      *****************************************************************
124400990707      * CARICAMENTO SUBFILE * SELEZIONE 2 - Rif. mittente
124500990707      *****************************************************************
124600990707     C     RIESEL2       BEGSR
124700990707      *
124800990707     c                   IF        *IN23 = *OFF
124900990707     C     V1Crmn        chain     TITAS32C                           24
125000990707     c                   ENDIF
125100990707      *
125200000211     c                   DOW       *in24=*Off  and  Wendpag <> 'S'
125300990707     C                   exsr      CTR_selez
125400990707     C     V1Crmn        reade     TITAS32C                               24
125500990707     C                   ENDDO
125600990707      *
125700990707     c                   ENDSR
125800990707      *****************************************************************
125900990707      * CARICAMENTO SUBFILE * SELEZIONE 3 - Rif. mittente + codice
126000990707      *****************************************************************
126100990707     C     RIESEL3       BEGSR
126200990707      *
126300990707     c                   IF        *IN23 = *OFF
126400990707     C     Krmn          chain     TITAS32C                           24
126500990707     c                   ENDIF
126600990707      *
126700990707     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
126800990707     C                   exsr      CTR_selez
126900990707     C     Krmn          reade     TITAS32C                               24
127000990707     C                   ENDDO
127100990707      *
127200990707     c                   ENDSR
127300990707      *****************************************************************
127400990707      * CARICAMENTO SUBFILE * SELEZIONE 4 - Rif. partner
127500990707      *****************************************************************
127600990707     C     RIESEL4       BEGSR
127700990707      *
127800070927     c***                movel(P)  V1Crpt        W020A
127900070927     c                   movel(P)  V1Crpt        W030A
128000990707      *
128100990707     c                   IF        *IN23 = *OFF
128200070927     C     W030A         chain     TITA438C                           33
128300000901     C  N33Ktas          chain     TITAS30C                           24
128400990707     c                   ENDIF
128500990707      *
128600000901     c                   DOW       *IN33 = *off
128700000901     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
128800990707     C                   exsr      CTR_selez
128900040217     c     Wendpag       cabeq     'S'           EndSel04
129000000901     C     Ktas          reade     TITAS30C                               24
129100990707     C                   ENDDO
129200040217     c     Wendpag       cabeq     'S'           EndSel04
129300070927     C     W030A         reade     TITA438C                               33
129400000901     C  N33Ktas          chain     TITAS30C                           24
129500000901     C                   ENDDO
129600990707      *
129700040217     c     EndSel04      ENDSR
129800990622      *****************************************************************
129900990707      * CARICAMENTO SUBFILE * SELEZIONE 5/6 - Cliente fatturazione e data fattura/non fatturate
130000990622      *****************************************************************
130100990707     C     RIESEL56      BEGSR
130200990622      *
130300990622     c                   IF        *IN23 = *OFF
130400990707     C                   IF        Wselez = 5
130500990707     C     Kfat1         setll     TITAS31C
130600990707     c                   ELSE
130700990707     C     kfat2         setll     TITAS31C
130800990622     c                   ENDIF
130900990825     c
131000990825     C                   IF        Wselez = 5
131100990707     C     KFAT1         reade     TITAS31C                               24
131200990825     c                   else
131300990825     C     kfat2         reade     TITAS31C                               24
131400990825     c                   endif
131500990707     c                   ENDIF
131600990622      *
131700990707     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
131800990622     C                   exsr      CTR_selez
131900990825     C                   IF        Wselez = 5
132000990707     C     KFAT1         reade     TITAS31C                               24
132100990825     c                   else
132200990825     C     kfat2         reade     TITAS31C                               24
132300990825     c                   endif
132400990622     C                   ENDDO
132500990622      *
132600990622     c                   ENDSR
132700990707      *****************************************************************
132800990707      * CARICAMENTO SUBFILE * SELEZIONE 7 - Codice cliente fatturazione e data fattura e n. fattura
132900990707      *****************************************************************
133000990707     C     RIESEL7       BEGSR
133100990707      *
133200990707     c                   IF        *IN23 = *OFF
133300990716     C     KFAT3         chain     TITAS31C                           24
133400990707     c                   ENDIF
133500990707      *
133600990707     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
133700990707     C                   exsr      CTR_selez
133800990716     C     KFAT3         reade     TITAS31C                               24
133900990707     C                   ENDDO
134000990707      *
134100990707     c                   ENDSR
134200060608      *****************************************************************
134300060608      * CARICAMENTO SUBFILE * SELEZIONE 17 - P.O, iva e numero fattura      ta fattura e n. fattura
134400060608      *****************************************************************
134500060608     C     RIESEL17      BEGSR
134600060608      *
134700060608     c                   IF        *IN23 = *OFF
134800060608     c                   if        v1cdft=0
134900060608     C     KFAT4         chain     TITAS40C                           24
135000060608     c                   else
135100060608     C     KFAT43        chain     TITAS40C                           24
135200060608     c                   endif
135300060608     c
135400060608     c                   ENDIF
135500060608      *
135600060608     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
135700060608     C                   exsr      CTR_selez
135800060608     c                   if        v1cdft=0
135900060608     C     KFAT4         reade     TITAS40C                               24
136000060608     c                   else
136100060608     C     KFAT43        reade     TITAS40C                               24
136200060608     c                   endif
136300060608     C                   ENDDO
136400060608      *
136500060608     c                   ENDSR
136600990707      *****************************************************************
136700990707      * CARICAMENTO SUBFILE * SELEZIONE 8 - data spedizione
136800990707      *****************************************************************
136900990707     C     RIESEL8       BEGSR
137000000718      *
137100990707     c                   IF        *IN23 = *OFF
137200990707     c                   movel     WV1Cdsd       Kaas
137300990707     c                   move      WV1Cdsd       Kmgs
137400010112     C                   eval      klinea = v1clp1
137500990707      *
137600000717     C     kdta          setll     TITAS39C
137700000717     C     v1cLP1        reade     TITAS39C                               24
137800990707     c                   ENDIF
137900990707      *
138000990707     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
138100990707     C                   exsr      CTR_selez
138200990707      * Se supero data limite fine lettura
138300990707     C                   If        Wdatasp > WV1Cdsa
138400990707     C                   eval      *IN24 = *on
138500990707     c                   Else
138600000717     C     v1cLP1        reade     TITAS39C                               24
138700990707     c                   Endif
138800990707     C                   ENDDO
138900000718      *
139000000718     c     endsbr8       ENDSR
139100990813      *****************************************************************
139200990813      * CARICAMENTO SUBFILE * SELEZIONE 9 - LNP e data spedizione
139300990813      *****************************************************************
139400991103     C*    RIESEL9       BEGSR
139500990813      *
139600991103     c*                  IF        *IN23 = *OFF
139700991103     C*    kspe9         setll     TITAS30C
139800991103     C*    Kspe9         reade     TITAS30C                               24
139900991103     c*                  ENDIF
140000990813      *
140100991103     c*                  DOW       *IN24 = *off  and  Wendpag <> 'S'
140200991103     C*                  exsr      CTR_selez
140300990813      * Se supero data limite fine lettura
140400991103     C*                  If        Wdatasp > WV1Cdsa
140500991103     C*                  eval      *IN24 = *on
140600991103     c*                  Else
140700991103     C*    Kspe9         reade     TITAS30C                               24
140800991103     c*                  Endif
140900991103     C*                  ENDDO
141000990813      *
141100991103     c*                  ENDSR
141200000222      * ------------------------------------------------------------------*
141300000222      *  CALCOLA CHECK DIGIT PER RIFERIMENTO DPD
141400000222      * ------------------------------------------------------------------*
141500000222     C     M1031         BEGSR
141600060607     c                   clear                   w0150
141700060607     c
141800060607     c                   move      v1cdpd        w003a             3
141900060607     c                   if        w003a=*blanks
142000000222      *
142100060609     C                   MOVEL(P)  v1cdpd        DPDBRC
142200000222      *
142300000222     c                   clear                   W0030a
142400000222     c                   clear                   w0030
142500000222      *
142600000222      * TOTALIZZO I VALORI, QUELLI NELLE POSIZIONI DISPARI PER 3
142700000222     c                   DO        11            xx
142800000222      *
142900000222     c     xx            div       2             W0030a
143000000222     c                   mvr                     Wresto
143100000222     c                   If        Wresto > *zeros
143200000222     c                   eval       W0030 = W0030 + (§11(xx) * 3)
143300000222     c                   Else
143400000222     c                   add       §11(xx)       W0030
143500000222     c                   Endif
143600000222      *
143700000222     c                   ENDDO
143800000222      *
143900000222     c     w0030         add       9             W0030a
144000000222     c                   move      0             W0030a
144100000222     c     W0030a        sub       w0030         dpdchd
144200000222     c
144300000222     C                   Z-ADD     DPDBRC        W0150
144400060607     c                   endif
144500000222      *
144600000222     c                   endsr
144700000211      *****************************************************************
144800000211      * CARICAMENTO SUBFILE * SELEZIONE 10 - Numero Parcel DPD
144900000211      *****************************************************************
145000000211     C     RIESEL10      BEGSR
145100000211      *
145200060607      * Ricerco sia import che export Export in TITA4
145300061120     c                   if        wgiro<>'2'
145400060529     c                   movel(p)  V1Cdpd        W014A
145500061120     c                   endif
145600000211      *
145700000211     c                   IF        *IN23 = *OFF
145800060529     C     W014A         chain     TITA437C                           33
145900000901     C  N33Ktas          chain     TITAS30C                           24
146000000211     c                   ENDIF
146100000211      *
146200061120     c     rsel10_tag    tag
146300061120     c*
146400000901     c                   DOW       *IN33 = *off
146500000901     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
146600000223     C                   exsr      ctrspeddpd
146700000223     C  N31              exsr      CTR_selez
146800040217     c     Wendpag       cabeq     'S'           EndSel10
146900000901     C     Ktas          reade     TITAS30C                               24
147000000901     C                   ENDDO
147100040217     c     Wendpag       cabeq     'S'           EndSel10
147200060529     C     W014A         reade     TITA437C                               33
147300000901     C  N33Ktas          chain     TITAS30C                           24
147400000901     C                   ENDDO
147500000211      *
147600060607      * Ricerco Import nel Rif mittente numerico  solo se immesso
147700060607      *  un numero di 11 cifre (visto che il new sarà alfa e lungo 14)
147800061120     c                   if        w0150>0 and wgiro<>'2'
147900060607     c                   IF        *IN23 = *OFF
148000060607     C     W0150         chain     TITAS32C                           24
148100060607     c                   ENDIF
148200000211      *
148300060607     c                   DOW       *in24=*Off  and  Wendpag <> 'S'
148400060607     C                   exsr      ctrspeddpd
148500060607     C  N31              exsr      CTR_selez
148600060607     C     W0150         reade     TITAS32C                               24
148700060607     C                   ENDDO
148800061120     c*
148900061120     c                   eval      w014a='0'+ %subst(v1cdpd:1:3)+'99'+
149000061120     c                             %subst(v1cdpd:4:8)
149100061120     C     W014A         chain     TITA437C                           33
149200061120     C  N33Ktas          chain     TITAS30C                           24
149300061120     c                   eval      wgiro='2'
149400061120     c                   goto      rsel10_tag
149500060607     c
149600060607     c                   ENDIF
149700000211      *
149800040217     c     EndSel10      ENDSR
149900000717      *****************************************************************
150000000717      * CARICAMENTO SUBFILE * SELEZIONE 11 - lnp + destinatario
150100000717      *****************************************************************
150200000717     C     RIESEL11      BEGSR
150300000717      *
150400000717     c                   IF        *IN23 = *OFF
150500000717      *
150600000717     C     kspe11        setll     TITAS37C
150700000717     C     V1Clp1        reade     TITAS37C                               24
150800000717     c                   ENDIF
150900000717      *
151000000717     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
151100000717     C                   exsr      CTR_selez
151200000717      * esco per destinatario non trovato
151300000717     c                   If        %SUBST(V1CRSD:1:XX) <> %SUBST(TASRSD:1:XX)
151400000717     C                   eval      *IN24 = *on
151500000717     c                   Else
151600000717     C     V1Clp1        reade     TITAS37C                               24
151700000717     c                   Endif
151800000717     C                   ENDDO
151900000717      *
152000000717     c                   ENDSR
152100000717      *****************************************************************
152200000718      * CARICAMENTO SUBFILE * SELEZIONE 12 - lnp + data + serie
152300000717      *****************************************************************
152400000717     C     RIESEL12      BEGSR
152500000718      *
152600000718     c                   IF        *IN23 = *OFF
152700000718     c                   movel     WV1Cdsd       Kaas
152800000718     c                   move      WV1Cdsd       Kmgs
152900000718     C                   move      V1Cser        WorkSerie
153000010112     C                   eval      klinea = v1clp1
153100000718     C     leggititas1   tag
153200000718     C     kdta1         setll     TITAS39C
153300000718     C     kdta1         reade     TITAS39C                               24
153400000718     C     *in24         ifeq      *on
153500000718     C     kdta          setgt     titas39c
153600000718     C     V1Clp1        reade     titas39c                               24
153700000718     C   24              goto      endsbr12
153800000718     c                   eval      Kaas = TASaas
153900000718     c                   eval      Kmgs = TASmgs
154000000718     C                   goto      leggititas1
154100000718     C                   endif
154200000718     C                   ENDIF
154300000718      *
154400000718     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
154500000718     C                   exsr      CTR_selez
154600000718     C     wdatasp       ifle      WV1Cdsa
154700000718     C     leggititas2   tag
154800000718     C     Kdta1         reade     TITAS39C                               24
154900000718     C     *in24         ifeq      *on
155000000718     C     kdta          setgt     titas39c
155100000718     C     V1Clp1        reade     titas39c                               24
155200000718     C   24              goto      endsbr12
155300000718     c                   eval      Kaas = TASaas
155400000718     c                   eval      Kmgs = TASmgs
155500000718     C     kdta1         setll     TITAS39C
155600000718     C                   goto      leggititas2
155700000718     C                   endif
155800000718     C                   else
155900000718     C                   seton                                        24
156000000718     C                   endif
156100000718     C                   ENDDO
156200000718      *
156300000718     c     endsbr12      ENDSR
156400000717      *****************************************************************
156500010104      * CARICAMENTO SUBFILE * SELEZIONE 13 - segnacollo
156600000717      *****************************************************************
156700000717     C     RIESEL13      BEGSR
156800010111
156900010112     c                   clear                   bolle
157000010112     C                   eval      *in15 = (v1clna > *zeros)
157100010112
157200010129      * Segnacollo univoco (segnacollo da TITAS o presente in TITAT)
157300010112     C                   if        v1flgs <> *blanks
157400010111
157500010109      * Lettura su TITAS (segnacolli sequenziali)
157600010112     C   15Ktat          setll     titas34c
157700010112     C  n15ktat1         setll     titas34c
157800010111     C                   do        *hival
157900010112     C   15Ktat          reade     titas34c
158000010112     C  n15ktat1         reade     titas34c
158100010111
158200010111     C                   if        %eof(titas34c) or wendpag = 'S'
158300010111     C                   leave
158400010111     C                   endif
158500010112      * Selezioni
158600010104     C                   exsr      CTR_selez
158700010111
158800010129      * Carico una schiera con le bolle che trovo
158900010129     C                   if        *in32 = *on
159000010111     c     dsbolle       lookup    bolle                                  30
159100010111     c                   IF        *IN30 = *off
159200010111     c                   eval      xb = 1
159300010111     c     *blanks       lookup    bolle(xb)                              30
159400010111     c   30              eval      bolle(xb) = dsbolle
159500010111     c                   ENDIF
159600010112     C                   endif
159700010111
159800010111     C                   enddo
159900010112
160000010129      * Lettura su TITAT
160100010129      * segnacolli non sequenziali o sequenziali con data consegna <> da TAS
160200010112     c   15ktat          setll     titat24c
160300010112     c  n15ktat1         setll     titat24c
160400010109     C                   do        *hival
160500010112     C   15ktat          reade     titat24c
160600010112     C  n15ktat1         reade     titat24c
160700010112     C                   if        %eof(titat24c) or Wendpag = 'S'
160800010109     C                   leave
160900010109     C                   endif
161000010112     C     ktats         chain     titas30c
161100010109     C                   if        %found(titas30c)
161200010112      * selezioni
161300010111     C                   exsr      CTR_selez
161400010111
161500010109      * Controllo se ho già scritto la bolla
161600010112     c   32dsbolle       lookup    bolle                                  30
161700010112     C   30              iter
161800010109
161900010112     C                   endif
162000010129     C                   enddo
162100010111
162200010111     C                   endif
162300010111
162400010129      * Segnacollo non univoco
162500010129      * Sequenziali: compreso tra segnac. da e segnac. a in TITAS
162600010129      * Non Sequenziali: presente in TITAT
162700010112     C                   if        v1flgs = *blanks
162800010111
162900010111      * Lettura su TITAS
163000010115     c                   IF        *IN23 = *OFF
163100010112     c                   movel     WV1cdsd       Kaas
163200010112     c                   move      WV1cdsd       Kmgs
163300010112     C                   move      V1Cnrr        WorkSerie
163400010112     C                   move      V1Cnsc        KSEGNA
163500010112     C                   eval      klinea = v1cfls
163600010112     C     leggitita     tag
163700010112     C     kdta2         setgt     TITAS35C
163800010112     C     kdta1         readpe    TITAS35C                               24
163900010111     C     *in24         ifeq      *on
164000010112     C     kdta          setgt     titas35c
164100010112     C     V1Cfls        reade     titas35c                               24
164200010115     C   24              goto      leggititat
164300010111     c                   eval      Kaas = TASaas
164400010111     c                   eval      Kmgs = TASmgs
164500010112     C                   goto      leggitita
164600010111     C                   endif
164700010115     C                   endif
164800010111      *
164900010111     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
165000010115     C                   if        tasncd > *zeros
165100010111     C                   exsr      CTR_selez
165200010129      * Carico una schiera con le bolle che trovo.
165300010129      * Solo serie > 0 perchè elaboro TITAT
165400010112     C                   if        *in32 = *on  and v1cnrr > *zeros
165500010112     c     dsbolle       lookup    bolle                                  30
165600010112     c                   IF        *IN30 = *off
165700010112     c                   eval      xb = 1
165800010112     c     *blanks       lookup    bolle(xb)                              30
165900010112     c   30              eval      bolle(xb) = dsbolle
166000010112     c                   ENDIF
166100010112     C                   endif
166200010115     C                   endif
166300010112
166400010112     C     wdatasp       ifle      WV1cdsa
166500010112     C     leggitita1    tag
166600010112     C     Kdta1         readpe    TITAS35C                               24
166700010111     C     *in24         ifeq      *on
166800010112     C     kdta          setgt     titas35c
166900010112     C     V1Cfls        reade     titas35c                               24
167000010115     C   24              goto      leggititat
167100010111     c                   eval      Kaas = TASaas
167200010111     c                   eval      Kmgs = TASmgs
167300010112     C     kdta2         setgt     TITAS35C
167400010112     C                   goto      leggitita1
167500010111     C                   endif
167600010111     C                   else
167700010111     C                   seton                                        24
167800010111     C                   endif
167900010111     C                   ENDDO
168000010112
168100010115     C     leggititat    tag
168200010112      * Lettura su TITAT
168300010129      * Per semplicità di pgm leggo anche eventuali segnac. sequenziali con
168400010129      * serie e data cons. <> da bolla che ho gia elaborato prima.
168500010112     C                   if        v1cnrr > *zeros
168600010112     c   15ktat          setll     titat24c
168700010112     c  n15ktat1         setll     titat24c
168800010112     C                   do        *hival
168900010112     C   15ktat          reade     titat24c
169000010112     C  n15ktat1         reade     titat24c
169100010112     C                   if        %eof(titat24c) or Wendpag = 'S'
169200010112     C                   leave
169300010112     C                   endif
169400010112     C     ktats         chain     titas30c
169500010112     C                   if        %found(titas30c)
169600010112      * selezioni
169700010112     C                   exsr      CTR_selez
169800010112      * Controllo se ho già scritto la bolla
169900010112     c   32dsbolle       lookup    bolle                                  30
170000010112     C   30              iter
170100010112
170200010112     C                   endif
170300010112     C                   enddo
170400010112     C                   endif
170500010111
170600010111     C                   endif
170700010111
170800000717      *
170900010115     c                   ENDSR
171000000901      *****************************************************************
171100000901      * CARICAMENTO SUBFILE * SELEZIONE 14 - Codice "CHI SONO"
171200000901      *****************************************************************
171300000901     C     RIESEL14      BEGSR
171400000901      *
171500000901     c                   IF        *IN23 = *OFF
171600000901     C     ktah          chain     TITAH31C                           33
171700000901     C  N33ktahxs        chain     TITAS30C                           24
171800000901     c                   ENDIF
171900000901      *
172000000901     c                   DOW       *in33=*Off
172100000901     c                   DOW       *in24=*Off  and  Wendpag <> 'S'
172200000901     C                   exsr      CTR_selez
172300040217     c     Wendpag       cabeq     'S'           EndSel14
172400000901     C     ktahxs        reade     TITAS30C                               24
172500000901     C                   ENDDO
172600040217     c     Wendpag       cabeq     'S'           EndSel14
172700000901     C     ktah          reade     TITAH31C                               33
172800000901     C  N33ktahxs        chain     TITAS30C                           24
172900000901     C                   ENDDO
173000000901      *
173100040217     c     EndSel14      ENDSR
173200010219      *****************************************************************
173300010219      * CARICAMENTO SUBFILE * SELEZIONE 15 - numero ORM
173400010219      *****************************************************************
173500010219     C     RIESEL15      BEGSR
173600010219
173700010219      * Ricerco ORM in TITA4
173800010219
173900010219     c                   IF        *IN23 = *OFF
174000010219     C     ds_orm        chain     TITA432C                           33
174100010219     C  N33Ktas          chain     TITAS30C                           24
174200010219     c                   ENDIF
174300010219      *
174400010219     c                   DOW       *IN33 = *off
174500010219     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
174600010219     C                   exsr      CTR_selez
174700040217     c     Wendpag       cabeq     'S'           EndSel15
174800010219     C     Ktas          reade     TITAS30C                               24
174900010219     C                   ENDDO
175000040217     c     Wendpag       cabeq     'S'           EndSel15
175100010219     C     ds_orm        reade     TITA432C                               33
175200010219     C  N33Ktas          chain     TITAS30C                           24
175300010219     C                   ENDDO
175400010219
175500040217     C     EndSel15      ENDSR
175600000223      *****************************************************************
175700000223      * CONTROLLO SE SPEDIZIONE DPD NON ANCORA SELZIONATA
175800000223      *****************************************************************
175900000223     C     CTRspeddpd    BEGSR
176000000223      *
176100000223     c     TASlnp        lookup    FILDPD                                 30
176200000223     c  N30TASlna        lookup    FILDPD                                 30
176300000223      *
176400000223     c                   IF        *IN30 = *OFF
176500000223     c                   eval        *in31 = *on
176600000223     C                   ELSE
176700000223      *
176800000223     c                   eval      DSaas = TASaas
176900000223     c                   eval      DSlnp = TASlnp
177000000223     c                   eval      DSnrs = TASnrs
177100000223     c                   eval      DSnsp = TASnsp
177200000223      *
177300000223     c     DSsped        lookup    SPED                                   31
177400000223     c                   IF        *IN31 = *off
177500000223     c                   eval      xx = 1
177600000223     c     *zeros        lookup    SPED(xx)                               30
177700000223     c                   eval      SPED(xx) = DSsped
177800000223     c                   ENDIF
177900000223      *
178000000223     c                   ENDIF
178100000223      *
178200000223     c                   ENDSR
178300040216      *****************************************************************
178400040216      * CARICAMENTO SUBFILE * SELEZIONE 16 - Rif. alfabetico
178500040216      *****************************************************************
178600040216     C     RIESEL16      BEGSR
178700040216      *
178800040216     c                   IF        *IN23 = *OFF
178900040216     C     V1Crma        chain     TITA435C                           33
179000040216     C  N33Ktas          chain     TITAS30C                           24
179100040216     c                   ENDIF
179200040216      *
179300040216     c                   DOW       *IN33 = *off
179400040216     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
179500040216     C                   exsr      CTR_selez
179600040217     c     Wendpag       cabeq     'S'           EndSel16
179700040216     C     Ktas          reade     TITAS30C                               24
179800040216     C                   ENDDO
179900040217     c     Wendpag       cabeq     'S'           EndSel16
180000040216     C     V1Crma        reade     TITA435C                               33
180100040216     C  N33Ktas          chain     TITAS30C                           24
180200040216     C                   ENDDO
180300040216      *
180400040217     c     EndSel16      ENDSR
180500990618      *****************************************************************
180600990618      * CONTROLLO SE C.A. SELEZIONABILE PER CARICAMENTO SUBFILE
180700990618      *****************************************************************
180800990618     C     CTR_selez     BEGSR
180900990618
181000010111     C                   eval      *in32 = *off
181100990623      * Compongo la data spedizione per confronti
181200990623     C                   eval      wdatasp = (TASaas * 10000) + TASmgs
181300990623
181400990622      * determino lunghezza stringa ricerca destinatario
181500990622     C                   IF        V1Crsd <> *blanks
181600990622     c     ' '           checkr    V1Crsd        xx
181700990622     c                   ENDIF
181800990707
181900990707      * imposto segnacollo finale se zero
182000010112     C                   IF        TASnca = *zeros
182100010112     c                   z-add     TASncd        TASnca
182200010112     c                   ENDIF
182300000717      * imposto serie
182400000717     C                   IF        V1Cser <> *blanks
182500000717     c                   move      V1Cser        WorkSerie         2 0
182600000717     c                   ENDIF
182700000717
182800010129      * Bolle SI/NO/SOLO Poste
182900010129      * Non controllo se P.O. partenza o P.O. segnacollo impostati
183000010129      * e se accetto tutte le bolle (flag = "S")
183100121025     C**********         eval      *in37 = *off
183200010129      *
183300121025     c*************      IF        V1Cfls = *zeros  and  V1Clp1 = *zeros
183400121025     c***********                                   and  V1Csnp <> 'S'
183500121025     C**** TASlnp        lookup    FILPOSTE                               30
183600000717      *
183700121025     c*****              IF        *IN30 = *on  and  V1Csnp = 'N'
183800121025     C****               eval        *in37 = *on
183900121025     C*****              ENDIF
184000010129      *
184100121025     c********           IF        *IN30 = *off  and  V1Csnp = 'P'
184200121025     C********           eval        *in37 = *on
184300121025     C*******            ENDIF
184400000717      *
184500121025     C*********          ENDIF
184600030529     C*
184700030529     C* Inserisco qui i controlli x verificare la data limite x considerare le
184800030529     C* bolle PT (tale controllo nn viene effettuato se l'utente è del gruppo
184900030529     C* EDP o ISP).
185000121025     C********           if        %subst(KPJBU:2:2) <> '05'
185100121025     C********           if        %subst(KNMUS:1:3) = 'EDP'
185200121025     C*********          else
185300121025     C***  tasLNP        lookup    FILPOSTE                               96
185400121025     C***********        if        *in96
185500121025     C***********        if        tasDCM >  0
185600121025     C***********        if        tasDCM >= wDataLimitePT
185700121025     C**********         else
185800121025     C**************     seton                                        37
185900121025     C**************     endif
186000121025     C**************     endif
186100121025     C***************    endif
186200121025     C***************    endif
186300121025     C*************      endif
186400030529     C*
186500990622
186600990618     c                   SELECT
186700990618      *
186800121025     C***********        WHEN      *in37 = *on                                  POSTE SI/NO
186900000717      *
187000000717     C                   WHEN      V1Cksc <> *zeros  AND  V1Cksc <> TASksc      CODICE FATTURAZIONE
187100990618      *
187200990622     C                   WHEN      WV1Cdsd > *zeros  AND                        DATA SPEDIZIONE
187300990623     c                             (Wdatasp < WV1Cdsd or Wdatasp > WV1Cdsa)
187400990618      *
187500990622     C                   WHEN      V1Crsd <> *blanks  AND                       DESTINATARIO
187600990622     c                             %SUBST(V1CRSD:1:XX) <> %SUBST(TASRSD:1:XX)
187700990618      *
187800990622     C                   WHEN      V1Clp1 > *zeros  AND  V1Clp1 <> TASlnp       P.O. PARTENZA
187900000717      *
188000000717     C                   WHEN      V1Cser <> *blanks AND WorkSerie <> TASnrs    SERIE
188100990622      *
188200990622     C                   WHEN      V1Clna > *zeros  AND  V1Clna <> TASlna       P.O. ARRIVO
188300040216      *
188400040216     C                   WHEN      V1Ccbo > *blanks AND  V1Ccbo <> TAScbo       CODICE BOLLA
188500990623      *
188600990623      * I controlli per segnacollo devono restare alla fine
188700010112     C                   WHEN      V1Cnsc > *zeros  AND                          SELEZ. SEGNACOLLO
188800010112     C                               (V1Cfls <> TASfls  OR  V1Cnrr <> TASnrs)   P.O. E SERIE SEGNAC.
188900990618      *
189000010115     C                   WHEN      V1Cnsc > *zeros  AND  tasfns <> 'S'  AND        NUMERO SEGNACOLLO
189100010112     C                             TASncd > *zeros  AND  V1Cnsc < TASncd                 DAL
189200990623      *
189300010115     C                   WHEN      V1Cnsc > *zeros  AND  tasfns <> 'S'  AND        NUMERO SEGNACOLLO
189400010112     C                             TASnca > *zeros  AND  V1Cnsc > TASnca                AL
189500010115      *
189600010115     C                   WHEN      V1Cnsc > *zeros  AND  tasfns <> 'S'  and        NUMERO SEGNACOLLO
189700010115     C                             (V1Cnsc > TASnca  OR  V1Cnsc < TASncd)               AL
189800990618      *
189900990618     c                   OTHER
190000990623      *
190100010111      * indicatore di comodo per caricare la schiera nella riesel13
190200010115     C                   eval      *in32 = *on
190300010115     C                   exsr      RICsbfl
190400990618      *
190500990618     C                   ENDSL
190600990618      *
190700990618     c                   ENDSR
190800990623      **********************************************************************
190900990623      * SCRITTURA RCD SBFL
191000990623      **********************************************************************
191100990623     C     RICsbfl       BEGSR
191200120517      *
191300120517      * - Già carico il n° max di rec. nel sfl !!!
191400120517     c                   if        Nrr1 = *hival
191500120517     c                   eval      $Full = *on
191600120517     c                   eval      Wendpag = 'S'
191700120517     c                   eval      V1Cmsg  = Msg(40)
191800120517     c                   eval      *in90 = *on
191900120517     c                   eval      *in28 = *on
192000120517     c                   leavesr
192100120517     c                   endif
192200990623      *
192300990618      * Carico
192400990622     c                   movel     *blanks       V2Copz
192500990622     c                   z-add     TASaas        V2Haas
192600990623     c                   z-add     TASaas        V2Cdsp
192700990623     c                   movel     TASmgs        V2Cdsp
192800990623     c     *mdy          move      V2Cdsp        DATA_eur
192900990623     c     *dmy          move      DATA_eur      V2Cdsp
193000990622     c                   z-add     TASlnp        V2Clnp
193100990622     c                   z-add     TASnrs        V2Cnrs
193200990622     c                   z-add     TASnsp        V2Cnsp
193300990622     c                   movel     TAStbl        V2Ctbl
193400990622     c                   z-add     TASlna        V2Clna
193500990622     c                   z-add     TASnft        V2Cnft
193600990622     c                   IF        TASdft = *zeros
193700990707     c                   clear                   V2Cdft
193800990622     c                   ELSE
193900990624     c     *iso          move      TASdft        DATA_eur
194000990707     c     *dmy          move      DATA_eur      W0060
194100990707     C                   eval      V2Cdft = %EDITC(W0060:'Y')
194200990622     c                   ENDIF
194300990622     c                   z-add     TASrmn        V2Crmn
194400990622     c                   z-add     TASksc        V2Cksc
194500990622     c                   z-add     TASctr        V2Cctr
194600990622     c                   movel     TAStsp        V2Ctsp
194700990622     c                   movel     TASrsd        V2Crsd
194800990707     c                   IF        TASdcm = *zeros
194900990707     c                   clear                   V2Cdcm
195000990707     c                   ELSE
195100990707     c     *iso          move      TASdcm        DATA_eur
195200990707     c     *eur          move      DATA_eur      W0080
195300990707     C                   eval      V2Cdcm = %EDITC(W0080:'Y')
195400990707     c                   ENDIF
195500990618      *
195600990622      * Ricavo ragione sociale mittente
195700990622     c                   clear                   V2Crsc
195800990708     c                   move      TASccm        W0040
195900990623     c                   IF        W0040 = 9999  or  W0040 = 8888
196000990623     c                                           or  W0040 = *zeros
196100990629     c                   eval      ktrc = 'M'
196200990707     c     KTAA          CHAIN     TITAA30C
196300990707     c                   IF        %FOUND
196400990622     c                   movel     TAArsc        V2Crsc
196500990618     c                   ENDIF
196600990622      *
196700990622     c                   ELSE
196800990622     c                   z-add     TASccm        I69kac
196900990622     c                   exsr      CTR_anagra
197000990707     C                   If         WO69ERR <> '1'
197100990622     c                   movel     ACOrag        V2Crsc
197200990622     c                   Endif
197300990622     c                   ENDIF
197400990618      *
197500990623     c                   add       1             NUMspedcar
197600990618     c                   add       1             NRR1
197700990623     c                   add       1             Wspedpag
197800990618      *
197900990622      * posizione cursore pagina nuova per ROLLUP carico 18 rcd per volta
198000990618     c                   SELECT
198100990623     c                   WHEN      Wspedpag = 1
198200990618     c                   eval      nrr23 = nrr1
198300990623     c                   WHEN      Wspedpag = 18
198400990618     c                   eval      Wendpag = 'S'
198500990618     c                   ENDSL
198600990618      *
198700990623     c                   write     SB50S02
198800990618      *
198900990622     c                   ENDSR
199000990618      **********************************************************************
199100990618      * EMISSIONE VIDEO 2
199200990618      **********************************************************************
199300990618     C     EMISD02       BEGSR
199400990618      *
199500990618     c                   SELECT
199600990618     c                   WHEN      *IN23 = *ON  and NRR23 > *zeros
199700990618     c                   z-add     nrr23         V2Cnrr
199800990618     c                   WHEN      *IN90 = *ON  and NRR1 > *zeros
199900990618     c                   z-add     nrr1          V2Cnrr
200000990618     c                   OTHER
200100990618     c                   z-add     1             V2Cnrr
200200990618     c                   ENDSL
200300990618      *
200400990730     c                   eval      *in20 = *on
200500990730     c                   eval      *in21 = *on
200600990618      *
200700990623     c                   write     SB50T01
200800990623     c                   write     SB50Z02
200900990623     c                   exfmt     SB50C02
201000990618      *
201100990730     c                   eval      *in90 = *off
201200990730     c                   eval      *in28 = *off
201300990623     c                   clear                   v1cmsg
201400990618      *
201500990618     C                   ENDSR
201600990618      **********************************************************************
201700990618      * CONTROLLI VIDEO 2
201800990618      **********************************************************************
201900990618     C     Ctrd02        BEGSR
202000990618      *
202100990624     c                   clear                   wselsped
202200990802     c                   clear                   nrr60
202300990618     c                   z-add     1             nrr2
202400990618      *
202500990706     c     NRR2          CHAIN     SB50S02
202600990618      *
202700990706     c                   DOW       %FOUND  and  *IN90 = *OFF
202800990618
202900990618     c                   SELECT
203000990618
203100990630     c                   WHEN      V2Copz = *blanks
203200990630
203300990630     c                   WHEN      V2Copz = '5'
203400990623     c                   exsr      SUB_opz05
203500990623     c                   clear                   V2Copz
203600990706     c                   eval      *in90 = (O51F03 = '1')                       per F3 torno al SFL
203700990802     c                   eval      *in60 = *on
203800990802     c                   z-add     NRR1          NRR60
203900041012      * se selezionata bolla vado a fine
204000110520     c                   if        not *in90
204100110520     c                   eval      *IN90 = (wselsped = '1' and o51f03 <> *on)
204200110520     c                   endif
204300990802
204400990705     c                   WHEN      V2Copz = '1'  and  V2Copz = V2Dsel           Selezione
204500990624     c                   z-add     V2Haas        D50aas
204600990624     c                   z-add     V2Clnp        D50lnp
204700990624     c                   z-add     V2Cnrs        D50nrs
204800990624     c                   z-add     V2Cnsp        D50nsp
204900990624     c                   movel     V2Ctbl        D50tbl
205000041008      * se richiamato dai reclami
205100041008     c                   If        sdsprm > 1
205200041008     c                   eval      IA1ins = 'S'
205300041008     c                   eval      IA1tor = 'S'
205400041008     c                   eval      IA1ogg = ds_OggSped
205500041008e   3c                   endif
205600041008      *
205700990624     c                   eval      *IN90 = *ON
205800990624     c                   eval      wselsped = '1'
205900990705
206000990705     c                   WHEN      V2Copz = '2'  and  V2Copz = V2Dsel           Gestione
206100990726     c                   exsr      SUB_opz02
206200990726     c                   clear                   V2Copz
206300120705     c* se non ci sono errori reimposto il tipo bolla se cambiato
206400120705     c                   if        d01err<>'1' and d01msg=*blanks and
206500120705     c                             d01tbl<>*blanks
206600120705     c                   movel     d01tbl        v2ctbl
206700120705     c                   endif
206800120705     c
206900990726     c                   eval      *in90 = (D01F03 = '1')                       per F3 torno al SFL
207000990802     c                   eval      *in60 = *on
207100990802     c                   z-add     NRR1          NRR60
207200990726     c                   if        D01msg <> *blanks
207300990726     c                   movel     D01msg        V1Cmsg
207400990730     c                   eval      *in90 = *on
207500990730     c                   eval      *in28 = *on
207600990726     c                   endif
207700990618
207800990623     c                   OTHER                                                  Opzione errata
207900990623     c                   movel     MSG(20)       V1Cmsg
208000990730     c                   eval      *in28 = *on
208100990802     c                   eval      *in90 = *on
208200990730     c                   eval      *in60 = *on
208300990802     c                   z-add     NRR1          NRR60
208400990618
208500990618     c                   ENDSL
208600990618      *
208700990623     c                   update    SB50S02
208800990802     c                   eval      *IN60 = *OFF
208900990618      *
209000990618     c                   IF        *IN90 = *OFF
209100990618     c                   add       1             nrr2
209200990706     c     NRR2          CHAIN     SB50S02
209300990618     c                   ENDIF
209400990618      *
209500990618     c                   enddo
209600990618      *
209700990802      * Se devo posizionarmi senza errore accendo IN90
209800990802     c                   IF        NRR60 <> *zeros
209900990802     c                   z-add     NRR60         NRR1
210000990802     c                   eval      *IN90 = *on
210100990802     c                   ENDIF
210200990802      *
210300990618     C                   ENDSR
210400990618      **********************************************************************
210500990623      * PREPARAZIONE DS PASSAGGIO PARAMETRI PER SELEZIONE 5
210600990618      **********************************************************************
210700990623     C     SUB_OPZ05     BEGSR
210800990618      *
210900990624     c                   clear                   TNSB51DS
211000990624      *
211100990705     c                   movel     I50OP0        I51OP0
211200990705     c                   movel     I50OP1        I51OP1
211300990705     c                   z-add     V2Haas        I51aas
211400990624     c                   z-add     V2Clnp        I51lnp
211500990624     c                   z-add     V2Cnrs        I51nrs
211600990624     c                   z-add     V2Cnsp        I51nsp
211700990624     c                   movel     V2Ctbl        I51tbl
211800990618      *
211900990624     C                   movel     TNSB51DS      Kpjbu
212000041008      * se richiamato dai recalmi passo anche
212100041008     c                   if         SDSprm > 1
212200041008      *
212300990624     c                   call      'TNSB51R'
212400990624     c                   parm                    KPJBA
212500041008     c                   parm                    FIDNA1ds
212600041012      *
212700041012      * se richiamato da reclami e selezionato la bolla vado a fine
212800041012if  1c                   if            SDSprm > 1
212900041012     c                             and IA1ins = 'S'
213000041012     c                   eval      Wselsped = '1'
213100041012e   1c                   endif
213200041008      *
213300041008     c                   else
213400041008      *
213500041008     c                   call      'TNSB51R'
213600041008     c                   parm                    KPJBA
213700041008      *
213800041008     c                   endif
213900990624      *
214000990624     C                   movel     KPJBU         TNSB51DS
214100990624      *
214200990618     C                   ENDSR
214300990726      **********************************************************************
214400990726      * PREPARAZIONE DS PASSAGGIO PARAMETRI PER SELEZIONE 2
214500990726      **********************************************************************
214600990726     C     SUB_OPZ02     BEGSR
214700990726      *
214800990726     c                   clear                   TNSB01DS
214900990726      *
215000990726     c                   movel     ' 02'         D01OP0
215100990726     c                   z-add     V2Haas        D01aas
215200990726     c                   z-add     V2Clnp        D01lnp
215300990726     c                   z-add     V2Cnrs        D01nrs
215400990726     c                   z-add     V2Cnsp        D01nsp
215500990726     c                   movel     V2Ctbl        D01tbl
215600990726      *
215700990726     C                   movel(P)  TNSB01DS      Kpjbu
215800990726      *
215900990726     c                   call      'TNSB52R'
216000990726     c                   parm                    KPJBA
216100990726      *
216200990726     C                   movel     KPJBU         TNSB01DS
216300990726      *
216400990726     C                   ENDSR
216500990621      *****************************************************************
216600990621      *  AGGANCIO ANAGRAFICHE
216700990621      *****************************************************************
216800990621     C     CTR_ANAGRA    BEGSR
216900990621      *
217000990707     c                   clear                   WO69ERR
217100990707      *
217200990621     C                   CALL      'TIBS69R'
217300990621     C                   PARM                    tibs69DS
217400990621     C                   PARM                    DS_cnaco
217500990621     C                   PARM                    DS_cnind
217600990621     C                   PARM                    DS_cnclp
217700990621     C                   PARM                    DS_fncls
217800990621      *
217900990621     C                   eval      wtibs69r = 'S'
218000990707     C                   eval      WO69ERR =  O69ERR
218100990621      * per il 1° giro è inizializzata nelle specifiche "D"
218200990621     C                   clear                   TIBS69DS
218300990621      *
218400990621     C                   ENDSR
218500990618      *****************************************************************
218600990618      *  Ricerca alfabetica
218700990618      *****************************************************************
218800990618     c     ricalf        begsr
218900990618      *
219000990618     C                   MOVEL     rsut          PAXdut                         Descrizione Azienda
219100990618     C                   Z-ADD     9             PAXsta                         9=Escludo Annullati
219200030529     C                   Z-ADD     DUTKCI        PAXccc                         Capoconto Clienti
219300000901     C                   z-add     1             PAXNUM
219400990618      *
219500000901     C                   CALL      'XALFA3BR'
219600990618     C                   PARM                    PAXDUT
219700990618     C                   PARM                    CODUT
219800990618     C                   PARM                    PAXDMT
219900990618     C                   PARM                    PAXCCC
220000990618     C                   PARM                    PAXSTA
220100990618     C                   PARM      *blanks       PAXFLR
220200990618     C                   PARM      *blanks       PAXDIT
220300000901     C                   PARM                    PAXNUM            2 0
220400000901     C                   PARM                    PAXKCM           80
220500000901     C                   PARM                    PAXKSM          140
220600000901     C                   PARM                    PAXKDM           60
220700990618      *
220800990618     c                   endsr
220900990805      **********************************************************************
221000990805      * CONTROLLI STANDARD    * data immessa a video *
221100990805      **********************************************************************
221200990805     C     CTR_data      BEGSR
221300990805      *
221400990805     c     *eur          TEST(DE)                WDATA_08
221500990805     c                   IF        %error = *off
221600990805     c     *eur          movel     WDATA_08      DATA_EUR
221700990805     c     *iso          movel     DATA_eur      WDATA_08G
221800990805     c                   ELSE
221900990805     c     *dmy          TEST(DE)                WDATA_08
222000990805     c                   IF        %error = *off  and  WDATA_02 = *zeros
222100990805     c     *dmy          movel     WDATA_06      DATA_EUR
222200990805     c     *eur          movel     DATA_eur      WDATA_08
222300990805     c     *iso          movel     DATA_eur      WDATA_08G
222400990805     c                   ELSE
222500990805     c                   eval      *IN28 = *on
222600990805     c                   ENDIF
222700990805     c                   ENDIF
222800990805      *
222900990805     c                   ENDSR
223000990618      **********************************************************************
223100990618      * CONTROLLI STANDARD    * linea *
223200990618      **********************************************************************
223300990618     C     CTR_linea     BEGSR
223400990618      *
223500990623     C                   IF        Wlinea = *zeros
223600990618     c                   MOVEL     MSG(6)        v1CMSG
223700990618     c                   eval      *in28 = *on
223800990618     c                   eval      *in90 = *on
223900990618     c                   ELSE
224000990618      *
224100990707     c     Wlinea        chain     azorg01l
224200990707     c                   if        not %FOUND  or  ORGfva <> *blanks
224300990618     c                   MOVEL     MSG(7)        v1CMSG
224400990618     c                   eval      *in28 = *on
224500990618     c                   eval      *in90 = *on
224600990618     c                   endif
224700990618     c                   ENDIF
224800990618      *
224900990618     c                   ENDSR
225000990618      **********************************************************************
225100990618      * CONTROLLI STANDARD    * anno *
225200990618      **********************************************************************
225300990618     C     CTR_anno      BEGSR
225400990618      *
225500990623     c                   IF        Wanno = *zeros
225600990618     c                   MOVEL     MSG(9)        v1CMSG
225700990730     c                   eval      *in90 = *on
225800990730     c                   eval      *in28 = *on
225900990618     C                   else
226000990618      *
226100990618      *   sistemo anno di due cifre
226200990623     C                   if        Wanno < 100  and Wanno > 60
226300990623     C                   ADD       1900          Wanno
226400990618     C                   endif
226500990623     C                   if        Wanno < 100  and Wanno <= 60
226600990623     C                   ADD       2000          Wanno
226700990618     C                   endif
226800990618      *
226900990618     C                   ENDIF
227000990618      *
227100990618     c                   ENDSR
227200030529     C*--------------------------------------------------------------------------------------------*
227300030529     C* CARTAB - CARICA I DATI TABELLATI
227400030529     C*--------------------------------------------------------------------------------------------*
227500121025     C**** CARTAB        BEGSR
227600030529     C*
227700030529     C* Partendo dalla data attuale reperisco la data limite olre la quale
227800030529     C* nn scendere nel considerare le bolle PT.
227900121025     C***                eval      wrkoggiiso = *loval
228000030529     C*
228100121025     C***                eval      tbeCOD = 'VPO'
228200121025     C*******            eval      tbeKE1 = 'VPO'
228300121025     C***  KTBE01P       chain     TNTBE01L
228400121025     C********           if        %found(TNTBE01L)
228500121025     C**********         eval      DVPO = tbeUNI
228600030529     C*
228700030529     C* A seconda che l'utente sia di SEDE o d Filiale considero due valori diversi x i giorni
228800030529     C* dalla data attuale entro i quali considerare le bolle PT >>> CONSEGNATE <<<
228900031114      * l'utente ASC99 è inserito come utente di sede, ma di fatto è un utente di filiale
229000121025     C**********         if        DUTPOU <> 046 or Knmus = 'ASC99     '
229100121025     C*************      eval      wGGLimPT = §VPOIPP
229200121025     C**************     else
229300121025     C**************     eval      wGGLimPT = §VPOIPS
229400121025     C***************    endif
229500030529     C*
229600030529     C* Se il valore in tabella è 9999 significa nessun blocco data limite
229700121025     C***********        if        wGGLimPT <> 9999
229800121025     C**************     time                    wrkoggiiso
229900121025     C**** wrkoggiiso    subdur    wGGLimPT:*DAYSwrkoggiiso
230000121025     C*****              endif
230100121025     C*******            endif
230200121025     C**** *iso          move      wrkoggiiso    wDataLimitePT
230300030529     C*
230400121025     C*****              ENDSR
230500030529     C*--------------------------------------------------------------------------------------------*
230600030529     C* REPDATIUTE - REPERISCE DATI UTENTE
230700030529     C*--------------------------------------------------------------------------------------------*
230800030529     C     REPDATIUTE    BEGSR
230900030529     C*
231000030529     C* Reimposta le variabili di lavoro
231100030529     C                   CLEAR                   TIBS34DS
231200030529     C                   CLEAR                   AZUTEDS
231300030529     C                   CLEAR                   DDATIUTE
231400030529     C*
231500030529     C* Reperisce i dati del profilo in ingresso
231600030529     C     *DTAARA       DEFINE    §azute        azuteds
231700030529     C     *DTAARA       DEFINE    §datiute      ddatiute
231800030529     C                   IN(E)     *DTAARA
231900030529     C                   IF        %Error                                       *NO errori
232000030529     C                   EVAL      I34Tla = 'L'
232100030529     C                   CALL      'TIBS34R'
232200030529     C                   PARM                    TIBS34DS
232300030529     C                   IN        *DTAARA
232400030529     C                   ENDIF
232500030529     C*
232600030529     C                   ENDSR
232700990618      *****************************************************************
232800990618      * ROUTINE INIZIALE
232900990618      *****************************************************************
233000990618     C     *INZSR        BEGSR
233100990618      *
233200990618     C     *ENTRY        PLIST
233300990618     C                   PARM                    KPJBA
233400041008     C                   PARM                    fidna1ds
233500030529     C*
233600030529     C* REPERISCE I DATI UTENTE
233700030529     C                   EXSR      REPDATIUTE
233800030529     C*
233900030529     C* CARICA I DATI TABELLATI
234000121025     C*******            EXSR      CARTAB
234100000718      * ! metodo
234200000718     C                   z-add     2             metodo            1 0
234300000718      *
234400990624      *--- DATI UTENTE/AZIENDA
234500030529     C                   Z-ADD     1             CODUT             1 0
234600000223
234700010129      * Carico schiera con P.O. DPD e P.O. POSTE
234800000223     c                   clear                   xx
234900000223     C     *loval        setll     azorg
235000000223     C                   read      azorg
235100000223      *
235200000223     C                   DOW       NOT %EOF
235300000223      *
235400010129     c                   movel     ORGde3        OG143
235500010129      *
235600010129     c                   SELECT
235700010129     c                   WHEN      ORGFVA <> *blanks
235800020211     C                   WHEN      §ogntw = 'DPD'
235900000223     C                   add       1             XX
236000000223     c                   z-add     ORGFIL        FILDPD(XX)
236100121025     C****               WHEN      §ogntw= 'PPT'
236200121025     C****               add       1             YY
236300121025     c****               z-add     ORGFIL        FILPOSTE(YY)
236400010129     C                   ENDSL
236500000223      *
236600000223     C                   read      azorg
236700000223     C                   ENDDO
236800990618
236900990618      * reperisco data e ora
237000990618     C                   TIME                    W0140
237100990618      * UDATE IN GGMMAAAA
237200990618     C                   MOVE      W0140         WDTGIO
237300990618      * UDATE IN AAAAMMGG
237400990805     C     *eur          MOVEL     WDTGIO        DATA_eur
237500990805     C     *iso          MOVEL     DATA_eur      dateu
237600990618
237700990618     C     kspe          KLIST
237800990622     C                   KFLD                    v1cAAS
237900990622     C                   KFLD                    v1cLNP
238000990622     C                   KFLD                    v1cNRS
238100990622     C                   KFLD                    v1cNSP
238200990618      *
238300990623     C     kspe1         KLIST
238400990623     C                   KFLD                    v1cAAS
238500990623     C                   KFLD                    v1cLNP
238600990623     C                   KFLD                    v1cNRS
238700990623     C                   KFLD                    v1cNSP
238800990623     C                   KFLD                    D50tbl
238900990813
239000990813     C     kspe9         KLIST
239100990813     C                   KFLD                    v1cAAS
239200990813     C                   KFLD                    v1cLP1
239300990623      *
239400990622     C     krmn          KLIST
239500990622     C                   KFLD                    v1cRMN
239600990622     C                   KFLD                    v1cCCM
239700990622      *
239800990707     C     kfat1         KLIST
239900990707     C                   KFLD                    v1cKSC
240000990707      *
240100990707     C     kfat2         KLIST
240200990622     C                   KFLD                    v1cKSC
240300990716     C                   KFLD                    Wv1cDFT
240400990622      *
240500990707     C     kfat3         KLIST
240600990622     C                   KFLD                    v1cKSC
240700990716     C                   KFLD                    Wv1cDFT
240800990622     C                   KFLD                    v1cNFT
240900060608     C     kfat4         KLIST
241000060608     C                   KFLD                    v1cfiv
241100060608     C                   KFLD                    v1cNFT
241200060608     C     kfat43        KLIST
241300060608     C                   KFLD                    v1cfiv
241400060608     C                   KFLD                    v1cNFT
241500060608     C                   KFLD                    Wv1cDFT
241600990622      *
241700990622     C     kdta          KLIST
241800010112     C                   KFLD                    klinea
241900000717     C                   KFLD                    KAAS
242000990622     C                   KFLD                    KMGS
242100000718      *
242200000718     C     kdta1         KLIST
242300010112     C                   KFLD                    klinea
242400000718     C                   KFLD                    KAAS
242500000718     C                   KFLD                    KMGS
242600000718     C                   KFLD                    WorkSerie
242700000718      *
242800000718     C     kdta2         KLIST
242900010112     C                   KFLD                    klinea
243000000718     C                   KFLD                    KAAS
243100000718     C                   KFLD                    KMGS
243200000718     C                   KFLD                    WorkSerie
243300000718     C                   KFLD                    KSEGNA
243400000223      *
243500000223     C     ktas          KLIST
243600000223     C                   KFLD                    ta4AAS
243700000223     C                   KFLD                    ta4LNP
243800000223     C                   KFLD                    ta4NRS
243900000223     C                   KFLD                    ta4NSP
244000990622      *
244100990629     C     ktaa          KLIST
244200990622     C                   KFLD                    tasAAS
244300990622     C                   KFLD                    tasLNP
244400990622     C                   KFLD                    tasNRS
244500990622     C                   KFLD                    tasNSP
244600990629     C                   KFLD                    Ktrc
244700990623      *
244800990623     C     ktat          KLIST
244900010112     C                   KFLD                    v1cfls
245000010112     C                   KFLD                    v1cnrr
245100010112     C                   KFLD                    v1cnsc
245200010112     C                   KFLD                    v1clna
245300000717      *
245400000717     C     ktat1         KLIST
245500010112     C                   KFLD                    v1cfls
245600010112     C                   KFLD                    v1cnrr
245700010112     C                   KFLD                    v1cnsc
245800000717      *
245900010112     C     ktats         KLIST
246000000717     C                   KFLD                    TATAAS
246100000717     C                   KFLD                    TATLNP
246200000717     C                   KFLD                    TATNRS
246300000717     C                   KFLD                    TATNSP
246400000717      *
246500000717     C     kspe11        KLIST
246600000717     C                   KFLD                    v1cLP1
246700000717     C                   KFLD                    v1cRSD
246800000717      *
246900000717     C     kspe12        KLIST
247000000717     C                   KFLD                    kAAS
247100000717     C                   KFLD                    v1cLP1
247200000717     C                   KFLD                    WorkSerie
247300000901      *
247400000901     C     ktah          KLIST
247500000901     C                   KFLD                    KKKTRC            1
247600000901     C                   KFLD                    V1Cnot
247700000901      *
247800000911     C                   MOVE      'C'           KKKTRC                         "Chi Sono"
247900000901      *
248000000901     C     ktahxs        KLIST
248100000901     C                   KFLD                    TAHAAS
248200000901     C                   KFLD                    TAHLNP
248300000901     C                   KFLD                    TAHNRS
248400000901     C                   KFLD                    TAHNSP
248500030529     C*
248600030529     C* Chiave su TNTBE01L - Parziale
248700121025     C***  KTBE01P       KLIST
248800121025     C***                KFLD                    TBECOD
248900121025     C***                KFLD                    TBEKE1
249000040216      *
249100040216      * Chiave su TABEL00F
249200040216     c     K03TAB00      klist
249300040216     c                   kfld                    TBLkut
249400040216     c                   kfld                    TBLcod
249500040216     c                   kfld                    TBLkey
249600990623
249700990618     C                   ENDSR
249800990618      *---------------------------------------------------------------------------------------------
249900990618** MSG  Lungh. 78                                                            *
250000120517TNSB50R- Livello procedura chiamante non valida. Avvertire il C.E.D.            1 - Not Used
250100990623TNSB50R- Codice cliente errato                                                  2
250200990623TNSB50R- Non richiedere il cod. mittente se non si è immesso il rif. mittente   3
250300990623TNSB50R- Impostate selezioni incongruenti                                       4
250400990623TNSB50R- Data fattura errata                                                    5
250500990623TNSB50R- Immettere la linea                                                     6
250600990623TNSB50R- Linea inesistente o annullata                                          7
250700120517Non richiedere data, numero fattura o P.O.IVA e le spedizioni NON fatturate     8
250800990623TNSB50R- Immettere l'anno spedizione                                            9
250900120517Se immesso numero FATTURA richiedere anche cod.Cliente Fatturazione o P.O. IVA  10
251000120517Se richiesta Data Fattura immettere Cod.fatturazione o P.O IVA e NumeroFattura  11
251100990623TNSB50R- Non richiedere sped. non fatturate se non è immesso il cod. fatturaz.  12
251200990623TNSB50R- Data spedizione errata                                                 13
251300990623TNSB50R- Immettere data spedizione DAL minore o uguale a data spedizione AL     14
251400990623TNSB50R- Immettere almeno una selezione valida                                  15
251500990623TNSB50R- Non richiedere la serie se non immessi anche P.O. e numero segnacollo  16
251600990623TNSB50R- Non richiedere il P.O. segnacollo se non immesso il numero segnacollo  17
251700990623TNSB50R- Non richiedere il numero segnacollo se non immesso il P.O. segnacollo  18
251800990623TNSB50R- Bolla esistente ma annullata                                           19
251900990623TNSB50R- Opzione non valida                                                     20
252000990623TNSB50R- Scorrimento indietro o in avanti oltre il primo o l'ultimo record      21
252100120517TNSB50R- Non trovata la spedizione richiesta                                    22-Not Used
252200990726TNSB50R- Immettere il numero spedizione                                         23
252300000714TNSB50R- Immettere un valore numerico                                           24
252400000714TNSB50R- Immettere anche il P.O. Partenza                                       25
252500010129TNSB50R- Non richiedere la serie, altre serie inserite                          26
252600000717TNSB50R- Immettere anche la data di spedizione                                  27
252700120517TNSB50R- Non richiedere la linea se non immessi anche il numero segnacollo      28-Not Used
252800000718TNSB50R- Immettere al massimo il mese successivo della Data Spedizione dal      29
252900120517TNSB50R- Immettere anche il P.O. Arrivo- (serie è > di 0)                       30-Not Used
253000010219TNSB50R- Immettere il numero ORM                                                31
253100010219TNSB50R- Immettere il p.o. emissione                                            32
253200010219TNSB50R- P.O. emissione inesistente o annullato                                 33
253300010219TNSB50R- Non richiedere serie o viaggio se non inserito il n.orm                34
253400040216TNSB50R- Codice bolla errato                                                    35
253500060609Se immesso P.O. IVA immettere anche il Numero Fattura                           36
253600060609Se immesso P.O. IVA non immettere il cod.cliente fatturazione e viceversa       37
253700060609Se immesso cod.Cliente Fatturazione e numero fattura immettere anche la DATA    38
253800060614Parcel DPD formalmente errato: o lungo 11 numerico o lungo 14 alfanumerico      39
253900121108Caricato il numero massimo di spedizioni a video, anche se ce ne sono altre...  40
254000060609**
2541000606090123456789
