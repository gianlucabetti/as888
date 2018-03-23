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
014100990802     D wselsped        S              1
014200990624     D wendpag         S              1
014300990618     D wtibs69r        s              1
014400990624     D W006A           S              6
014500070927     D*** W020A           S             20
014600070927     D W030A           S             30
014700060529     D W014A           S             14
014800990618     D xx              S              3  0
014900010129     D yy              S              3  0
015000990805     D WDATA_08G       S              8  0
015100000222     D W0030           S              3  0
015200000222     D W0030a          S              3  0
015300000222     D Wresto          S              3  0
015400990623     D W0040           S              4  0
015500990707     D W0060           S              6  0
015600990707     D W0080           S              8  0
015700000222     D W0140           S             14  0
015800000222     D W0150           S             15  0
015900990618     D Wdtgio          S              8  0
016000990618     D dateu           S              8  0
016100990623     D NUMspedcar      S              4  0
016200990618     D nrr1            S              4  0                                      Assoluto
016300990618     D nrr2            S              4  0                                      Comodo x lettura
016400990618     D nrr23           S              4  0                                      1° rcd pag x rollup
016500990802     D nrr60           S              4  0                                      Assoluto
016600990623     D Wspedpag        S              4  0
016700000211     D Wselez          S              2  0
016800010109     D xb              S              4  0
016900990618
017000990618     D DATA_eur        S               D   DATFMT(*eur)
017100061120     D wgiro           S              1
017200030529     D*
017300030529     D* VARIABILI DI WRK X CONSIDERAZIONI SU BOLLE PT
017400030529     D wrkoggiiso      s               d
017500030529     D wDataLimitePT   s              8  0
017600030529     D wGGLimPT        s              4  0
017700990618      *
017800990618      *   S C H I E R E
017900060609     D MSG             S             78    DIM(39) CTDATA PERRCD(1)             MSG VIDEO
018000060609     D NUM             S              1    DIM(10) CTDATA PERRCD(10)            MSG VIDEO
018100000223     D SPED            S             16  0 DIM(90)
018200000223     D FILDPD          S              3s 0 DIM(20)
018300010129     D FILPOSTE        S              3s 0 DIM(20)
018400010109     D bolle           S             18    DIM(999)
018500060609     D w11             S              1    DIM(11)
018600990618      *
018700990618      *   D S   I N T E R N E / E S T E R N E
018800990630      *
018900000222     D                 DS
019000000222     D  §11                    1     11  0 dim(11)
019100000222     D  dpdchd                12     12  0
019200000222     D  dpdbrc                 1     12  0
019300000222      *
019400990805     D                 DS                  INZ
019500990805     D  WDATA_02               1      2  0
019600990805     D  WDATA_06               3      8  0
019700990805     D  WDATA_08               1      8  0
019800000223      *
019900000223     D                 DS                  INZ
020000000223     D  dsAAS                  1      4  0
020100000223     D  dsLNP                  5      7  0
020200000223     D  dsNRS                  8      9  0
020300000223     D  dsNSP                 10     16  0
020400000223     D  dsSPED                 1     16  0
020500010109
020600010111     D dsbolle         DS
020700010111     D  TASAAS
020800010111     D  TASLNP
020900010111     D  TASNRS
021000010111     D  TASNSP
021100010111     D  TASTBL
021200010219
021300010219     D ds_orm          DS                  inz
021400010219     D  v1cpoe                 1      3  0
021500010219     D  v1cnsr                 4      5  0
021600010219     D  v1cnor                 6     12  0
021700010219     D  v1cnrv                13     14  0
021800990618      *
021900041008      *?definizione oggetto reclamo come spedizione?
022000041008     d ds_OggSped      ds                  inz
022100041008     d  V2Clnp                             inz
022200041008     d  V2Cnrs                             inz
022300041008     d  V2Cnsp                             inz
022400041008     d  V2Haas                             inz
022500041008      *
022600990623     D KPJBA         E DS
022700041008     d FIDNA1ds      e ds
022800000717     D OG143         E DS
022900990726     D TNSB01DS      E DS                  INZ
023000990726     D TNSB50DS      E DS                  INZ
023100990624     D TNSB51DS      E DS                  INZ
023200990621     d TIBS69DS      E DS                  INZ
023300990618     d DS_cnaco      E DS                  extname(CNACO00F)
023400990618     d DS_cnind      E DS                  extname(CNIND00F)
023500990618     d DS_cnclp      E DS                  extname(CNCLP00F)
023600990618     d DS_fncls      E DS                  extname(FNCLS00F)
023700030529     D*
023800030529     D* DS X REPERIMENTO VARIABILI PGM OPERATIVI
023900030529     D DVPO          E DS
024000060608     D DSQC2         E DS
024100030529     D*------------
024200030529     D* DS REPERIMENTO DATI UTENTE
024300030529     D*------------
024400030529     D TIBS34DS      E DS                                                       *Profili utente
024500030529     D DDATIUTE      E DS                                                       *Dati utente
024600030529     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
024700990618      *
024800041008      *
024900041008     d Status         sds
025000041008     d  SDSprm           *parms
025100041008     D**************  SDS
025200990628     D  VTCPGM                 1     10
025300990727      *  titolo videata (lunghezza massima 32)
025400990727     D TIT_I           C                   CONST('* INTERROGAZIONE BOLLE DI SED-
025500990727     D                                     E *')
025600990727     D TIT_G           C                   CONST('*    GESTIONE BOLLE DI SEDE  -
025700990727     D                                       *')
025800060608     D FIV_Cost        C                   CONST('???=InizMese  ???=FineMese')
025900990618
026000990618      *------------------------------------------------------------------------*
026100990618      *---------------  C A L C O L O  ----------------------------------------*
026200990618      *------------------------------------------------------------------------*
026300990623
026400990623      * Controllo parametri ricevuti
026500990623     c                   clear                   TNSB50DS
026600990623     c                   SELECT
026700990623      *
026800990727     c                   WHEN       %SUBST(KPJBU:2:2) = '00'                    PArzializzazione
026900990623     c                   move      wdtgio        v1caas
027000990623     c                   movel     Kpjbu         I50OP0
027100990727     c                   movel     TIT_I         VTCtit
027200990623      *
027300990727     c                   WHEN       %SUBST(KPJBU:2:2) = '01'                    Parziali. con scelta
027400990623     c                   move      wdtgio        v1caas
027500990623     c                   movel     Kpjbu         I50OP0
027600990727     c                   movel     TIT_I         VTCtit
027700990705     c                   eval      v2dsel = '1'
027800990705      *
027900990705     c                   WHEN       %SUBST(KPJBU:2:2) = '02'                    Parzializzazione
028000990705     c                   move      wdtgio        v1caas                         per gestione
028100990705     c                   movel     Kpjbu         I50OP0
028200990727     c                   movel     TIT_G         VTCtit
028300990705     c                   eval      v2dsel = '2'
028400990623      *
028500990623     c                   WHEN       %SUBST(KPJBU:2:2) = '05'                    Visualizza spediz.
028600990623     c                   movel     Kpjbu         TNSB50DS
028700990623     c                   z-add     1             Wselez
028800990623     c                   z-add     D50aas        V1Caas
028900990623     c                   z-add     D50lnp        V1Clnp
029000990623     c                   z-add     D50nrs        V1Cnrs
029100990623     c                   z-add     D50nsp        V1Cnsp
029200990727     c                   movel     TIT_I         VTCtit
029300990623      *
029400990623     c                   OTHER                                                  opzione errata
029500990623     c                   eval      O50err = '1'
029600990623     c                   movel     MSG(1)        O50msg
029700990623     c                   movel     TNSB50DS      KPJBU
029800990623     c                   GOTO      FINE
029900990623     c                   ENDSL
030000000714
030100000714     C                   eval      v1csnp = 'N'
030200060608     c* Impostazione campi della prima videata
030300060608     c                   EXSR      IMPO
030400000714
030500990623      * Se richiesta la visualizzazione di una spedizione passo al caricamento del subfile
030600990624     c                   IF        %SUBST(I50OP0:2:2) <> '05'
030700990623      *
030800990618     C     for01         tag
030900990623      *
031000990618      * Emissione VIDEO1
031100990618     c                   write     SB50T01
031200990623     c                   exfmt     SB50D01
031300990623      *
031400990618      * Inizializzo Campi ed Indicatori
031500010219     c     40            do        55            XX
031600990618     c                   eval      *IN(XX) = *OFF
031700990618     c                   enddo
031800990618     c                   clear                   V1Cmsg
031900990618     c                   eval      *IN28 = *off
032000990618     c                   eval      *IN90 = *off
032100990623      *
032200990618      * f3=Fine
032300990618     c   kc              goto      fine
032400990623      *
032500990618      * Controlli 1a videata
032600990618     c                   exsr      ctrd01
032700990623      *
032800990622     c                   IF        *IN28 = *ON  or  *IN90 = *ON
032900990622     c                   goto      for01
033000990622     c                   ENDIF
033100990623      *
033200990623     c                   ENDIF
033300990623
033400990618      * Carico subfile
033500990618     C                   eval      *in23 = *off
033600990618     C                   clear                   nrr1
033700990623     c                   clear                   NUMspedcar
033800000223     c                   clear                   sped
033900061120     c                   clear                   wgiro
034000990618      *
034100990624     C                   exsr      RIEd02
034200990729      *
034300990729      * EMISSIONE SUBFILE O ENTRATA DIRETTA NELLA SPEDIZIONE
034400990624     c                   SELECT
034500990708      *
034600990729      * Trovati 1 + n record validi: emetto il subfile.
034700990706     C                   WHEN       NRR1 > 1
034800990729      *
034900990729      * Trovato 1 record valido. Interrogazione per selezione emetto il subfile
035000990729     C                   WHEN       NRR1 = 1  and  %SUBST(I50OP0:2:2) = '01'
035100990708      *
035200990729      * Trovato 1 record valido. Gestione della spedizione, se torna con errore emetto il subfile
035300990729     C                   WHEN       NRR1 = 1  and  %SUBST(I50OP0:2:2) = '02'
035400990729     c                   exsr      SUB_opz02
035500990729     c                   If        D01msg <> *blanks
035600990729     c                   movel     D01msg        V1Cmsg
035700990729     c                   eval      *in90 = *on
035800990730     c                   eval      *in28 = *on
035900990729     c                   Else
036000990729     c                   clear                   SB50D01
036100990729     c                   move      wdtgio        v1caas
036200000714     c                   eval      v1csnp = 'N'
036300990729     c                   clear                   WV1Cccm
036400990729     c                   clear                   WV1Cksc
036500060608     c                   eval      v1dfiv=savfiv
036600010112     c                   goto      FOR01
036700990729     c                   Endif
036800990729      * Trovato 1 record valido. Interrogazione, visualizzo la spedizione.
036900990729      *  Se chiamato con opz 05 torno al chiamante
037000990624     C                   WHEN       NRR1 = 1
037100990624     c                   exsr      SUB_opz05
037200990706     c                   clear                   SB50D01
037300990706     c                   move      wdtgio        v1caas
037400000714     c                   eval      v1csnp = 'N'
037500990707     c                   clear                   WV1Cccm
037600990707     c                   clear                   WV1Cksc
037700060608     c                   eval      v1dfiv=savfiv
037800990708     c                   If        %SUBST(I50OP0:2:2) = '05'
037900990708     c                   goto      FINE
038000990708     c                   Else
038100041008      * se richiamato da reclami e selezionato la bolla vado a fine
038200041008if  1c                   if            SDSprm > 1
038300041008     c                             and IA1ins = 'S'
038400041008     c                   goto      FINE
038500041008e   1c                   endif
038600041008      *
038700010112     c                   goto      FOR01
038800990708     c                   Endif
038900990708      *
039000990708      * Trovati 0 record validi e chiamato con opzione 05: ritorno al chiamante una segnalazione.
039100990729     c                   WHEN       NRR1 = *zeros and %SUBST(I50OP0:2:2) = '05'
039200990624     c                   eval      O50err = '1'
039300990726     c                   movel     msg(22)       O50msg
039400990624     c                   movel     TNSB50DS      KPJBU
039500990624     c                   goto      FINE
039600990708      *
039700990708      * Trovati 0 record validi e NON chiamato con opzione 05: emetto segnalazione e torno a VIDEO 1
039800990729     c                   WHEN       NRR1 = *zeros
039900990624     C     *INKL         DOWEQ     *OFF
040000990624     c                   write     SB50T01
040100990624     C                   exfmt     SB50D03
040200990624     C                   ENDDO
040300010112     c                   goto      FOR01
040400990624      *
040500990624     C                   ENDSL
040600990618
040700990618      * Emissione VIDEO2
040800990618     C     for02         tag
040900990618     C                   exsr      EMISd02
041000990618
041100990618      * F03 = Fine
041200990618     c   kc              goto      fine
041300990618
041400990708      * F12 = Precedente - Se chiamato con opzione "05" torno al chiamante
041500990707     c                   IF        *inKL = *ON
041600990708     c                   If        %SUBST(I50OP0:2:2) = '05'
041700990708     c                   goto      FINE
041800990708     c                   Else
041900990707     c                   clear                   SB50D01
042000990707     c                   move      wdtgio        v1caas
042100000714     c                   eval      v1csnp = 'N'
042200990707     c                   clear                   WV1Cccm
042300990707     c                   clear                   WV1Cksc
042400060608     c                   eval      v1dfiv=savfiv
042500010112     c                   goto      for01
042600990708     c                   Endif
042700990707     c                   ENDIF
042800990618
042900990618      * ROLLUP = Carico il subfile
043000990623     c                   IF        *IN23 = *ON
043100990618     c                   exsr      Ried02
043200990623     c                   If        Wspedpag = *zeros
043300990623     c                   movel     msg(21)       V1Cmsg
043400990618     c                   eval      *in28 = *on
043500990618     c                   Endif
043600990618     c                   GOTO      for02
043700990618     c                   ENDIF
043800990618
043900990618      * Controlli 2a videata
044000990618     c                   exsr      ctrd02
044100990618
044200990624      * Se selezionata una spedizione (Opz. 1) vado a fine altrimenti riemetto video 2
044300990624     c                   IF        Wselsped = '1'
044400990624     c                   movel     TNSB50DS      KPJBU
044500990624     c                   ELSE
044600990618     c                   goto      for02
044700990624     C                   ENDIF
044800990618
044900990618     c     fine          tag
045000990618
045100990618      * Chiudo pgm aperti
045200990618     C                   IF        Wtibs69r  <>  *BLANKS
045300990618     C                   eval      I69TLA  = 'C'
045400990618     C                   call      'TIBS69R'
045500990618     C                   parm                    TIBS69DS
045600990624     c                   ENDIF
045700990618
045800990624      * Imposto campi per uscita con F03
045900990624     c                   IF        *INKC = *ON
046000990624     c                   movel     TNSB50DS      W006A
046100990624     c                   clear                   TNSB50DS
046200990624     c                   movel     W006A         TNSB50DS
046300990624     c                   movel     '1'           O50F03
046400040311     c                   movel     TNSB50DS      KPJBU
046500990624     c                   ENDIF
046600990624
046700990618     c                   EVAL      *INLR = *ON
046800990618      **********************************************************************
046900060608      * Impostazione campi della prima videata
047000990618      **********************************************************************
047100060608     C     IMPO          BEGSR
047200060608     c* Chain tabella QC recod 2 per avere i libri iva
047300060608     c                   movel     CodUt         TBLkut
047400060608     c                   movel     'QC'          TBLcod
047500060608     c                   movel(p)  '2'           TBLkey
047600060608     c     K03TAB00      chain     TABEL
047700060608     c                   if        %found(tabel00f)
047800060608     c                   movel     tbluni        dsqc2
047900060608     c                   eval      v1dfiv=fiv_cost
048000060608     c                   eval      %subst(v1dfiv:1:3)=(%editc(§qcfiv: 'X'))
048100060608     c                   eval      %subst(v1dfiv:15:3)=(%editc(§qcfi2: 'X'))
048200060608     c                   endif
048300060608     c* salvo la stringa
048400060608     c                   movel     v1dfiv        savfiv
048500060608     c                   ENDSR
048600060608      **********************************************************************
048700060608      * CONTROLLI VIDEO 1
048800060608      **********************************************************************
048900060608     C     Ctrd01        BEGSR
049000990618
049100990621     C                   clear                   wselez
049200990621
049300990623      *  NUMERO SPEDIZIONE
049400990623     c                   IF        V1Cnsp <> *zeros  or  V1Clnp <> *zeros
049500990623      * Numero
049600990618     c                   IF        V1Cnsp = *zeros
049700990618     c                   eval        *IN40 = *on
049800990618     c                   eval        *IN28 = *on
049900990618     c                   eval        *IN90 = *on
050000990726     c                   movel     MSG(23)       V1Cmsg
050100990618     c                   ELSE
050200990623      * P.O
050300990623     C                   z-add     V1Clnp        Wlinea
050400990618     c                   exsr      CTR_linea
050500990618     c                   IF        *IN28 = *ON
050600990618     c                   eval        *IN40 = *on
050700990618     C                   ELSE
050800990623      * Anno
050900990623     c                   z-add     V1Caas        Wanno
051000990618     c                   exsr      CTR_anno
051100990618     c                   IF        *IN28 = *ON
051200990618     c                   eval        *IN41 = *on
051300990618     c                   else
051400990623     c                   z-add     Wanno         V1Caas
051500990618     c                   ENDIF
051600990618      *
051700990618     c                   ENDIF
051800990621     c                   ENDIF
051900990623     c                   ENDIF
052000990621      *
052100990621      * Controllo selezione
052200990621     c                   SELECT
052300990621     c                   WHEN      *IN28 = *ON
052400990621     c                   WHEN      V1Cnsp > *zeros
052500990621     c                   z-add     1             Wselez
052600990621     c                   ENDSL
052700990621      *
052800990621     c   28              goto      EndCTRD01
052900010219
053000010219      *  NUMERO ORM
053100010219     c                   IF        ds_orm <> *zeros
053200010219
053300010219      * obbligatorio il numero orm se inserito p.o.emissione
053400010219     C                   if        v1cpoe <> *zeros and v1cnor = *zeros
053500010219     c                   eval        *IN54 = *on
053600010219     c                   eval        *IN28 = *on
053700010219     c                   movel     MSG(31)       V1Cmsg
053800010219     C                   goto      endctrd01
053900010219     C                   endif
054000010219
054100010219      * obbligatorio il p.o. emissione se inserito il numero orm
054200010219     c                   if        v1Cpoe = *zeros  and  v1cnor <> *zeros
054300010219     c                   eval        *IN55 = *on
054400010219     c                   eval        *IN28 = *on
054500010219     c                   movel     MSG(32)       V1Cmsg
054600010219     C                   goto      endctrd01
054700010219     C                   endif
054800010219
054900010219      * già impostata una selezione
055000010219     c                   if        Wselez > *zeros
055100010219     c                   movel     MSG(4)        V1Cmsg
055200010219     c                   eval      *IN55 = *on
055300010219     c                   eval      *IN28 = *on
055400010219     C                   goto      endctrd01
055500010219     C                   endif
055600010219
055700010219      * non si può inserire la serie senza il p.o.e numero orm
055800010219     c                   if        V1Cnsr > *zeros  and
055900010219     c                             V1Cpoe = *zeros  and V1Cnor = *zeros
056000010219     c                   eval        *IN54 = *on
056100010219     c                   eval        *IN28 = *on
056200010219     c                   movel     MSG(34)       V1Cmsg
056300010219     C                   goto      endctrd01
056400010219     C                   endif
056500010219
056600010219      * non si può inserire il viaggio senza il p.o.e numero orm
056700010219     c                   if        V1Cnrv > *zeros  and
056800010219     c                             V1Cpoe = *zeros  and V1Cnor = *zeros
056900010219     c                   eval        *IN54 = *on
057000010219     c                   eval        *IN28 = *on
057100010219     c                   movel     MSG(34)       V1Cmsg
057200010219     C                   goto      endctrd01
057300010219     C                   endif
057400010219
057500010219      * Controlla il p.o.emissione
057600010219     C                   z-add     V1cpoe        Wlinea
057700010219     c                   exsr      CTR_linea
057800010219     c                   eval      *IN55 = (*in28 = *on)
057900010219     c   28              movel     MSG(33)       V1Cmsg
058000010219     C   28              goto      endctrd01
058100010219
058200010219      * imposta la selezione da fare
058300010219     C                   z-add     15            Wselez
058400010219     c                   ENDif
058500010112
058600010112      * SEGNACOLLO
058700010112     C                   if        v1cfls <> *zeros or v1cnsc <> *zeros
058800010112
058900010112      * obbligatorio il numero segnacollo se inserito p.o.segnacollo
059000010112     C                   if        v1cfls <> *zeros and v1cnsc =  *zeros
059100010112     c                   eval        *IN51 = *on
059200010112     c                   eval        *IN28 = *on
059300010112     c                   movel     MSG(17)       V1Cmsg
059400010112     C                   goto      endctrd01
059500010112     C                   endif
059600010112
059700010112      * obbligatorio il p.o. segnacollo se inserito il numero segnacollo
059800010112     c                   if        v1Cfls = *zeros  and  v1cnsc <> *zeros
059900010112     c                   eval        *IN51 = *on
060000010112     c                   eval        *IN28 = *on
060100010112     c                   movel     MSG(18)       V1Cmsg
060200010112     C                   goto      endctrd01
060300010112     C                   endif
060400010112
060500010112      * già impostata una selezione
060600010112     c                   if        Wselez > *zeros
060700010112     c                   movel     MSG(4)        V1Cmsg
060800010112     c                   eval      *IN51 = *on
060900010112     c                   eval      *IN28 = *on
061000010112     C                   goto      endctrd01
061100010112     C                   endif
061200010112
061300010112      * Controlla il p.o.segnacollo
061400010112     C                   z-add     V1cfls        Wlinea
061500010112     c                   exsr      CTR_linea
061600010112     c                   eval      *IN51 = (*in28 = *on)
061700010112     c   28              movel     MSG(7)        V1Cmsg
061800010112     C   28              goto      endctrd01
061900010112
062000010112      * imposta la selezione da fare
062100010112     C                   z-add     13            Wselez
062200010112     c                   ENDif
062300010112
062400010112      * non si può inserire la serie senza il p.o.e numero segnacollo
062500010112     c                   if        V1Cnrr > *zeros  and
062600010112     c                             V1Cfls = *zeros  and V1Cnsc = *zeros
062700010112     c                   eval        *IN51 = *on
062800010112     c                   eval        *IN28 = *on
062900010112     c                   movel     MSG(16)       V1Cmsg
063000010112     C                   goto      endctrd01
063100010112     C                   endif
063200990618
063300000901      * CODICE "CHI SONO"
063400000901     C     V1Cnot        IFNE      *blanks
063500000901     C     Wselez        IFEQ      *zeros
063600000901     C                   Z-ADD     14            Wselez
063700000901     C                   ELSE
063800000901     c                   movel     MSG(4)        V1Cmsg
063900000901     c                   eval      *IN40 = *on
064000000901     c                   eval      *IN28 = *on
064100000901     C                   ENDIF
064200000901     C                   ENDIF
064300000901      *
064400000901     c   28              goto      EndCTRD01
064500000901
064600040216      * RIFERIMENTO NUMERICO MITTENTE e CODICE CLIENTE MITTENTE
064700990618     c                   SELECT
064800990618      *    se immessa rag. sociale e non il codice mittente richiamo il pgm di ricerca alfabetica
064900990618     c                   WHEN      V1Cccm = *zeros  and  V1Dccm <> *blanks
065000990618     c                   movel(P)  V1Dccm        PAXdmt
065100990618     c                   exsr      ricalf
065200990618     C                   If        PAXsta > -1
065300000901     C                   movel     paxksm        V1Cccm
065400990621     c                   movel(P)  PAXdmt        V1Dccm
065500990618     C                   Endif
065600990618     c                   eval      *IN90 = *on
065700990621      *    se immesso il codice mittente lo decodifico
065800990618     c                   WHEN      V1Cccm > *zeros  and  V1Cccm <> WV1Cccm
065900990621     c                   clear                   V1Dccm
066000990621     c                   move      V1Cccm        w0040
066100990621     c                   IF        W0040 <> 9999  and  W0040 <> 8888
066200990621     c                   z-add     V1Cccm        I69kac
066300990621     c                   exsr      CTR_anagra
066400990707     C                   If         WO69ERR =  '1'
066500990623     c                   movel     MSG(2)        V1Cmsg
066600990621     c                   eval      *IN43 = *on
066700990621     c                   eval      *IN28 = *on
066800990621     c                   Else
066900990621     c                   eval      *IN90 = *on
067000990621     c                   movel     ACOrag        V1Dccm
067100990621     c                   Endif
067200990621     c                   ENDIF
067300990618      *    se inserito il codice mittente e non il riferimento mittente errore
067400990623     c                   WHEN      V1Cccm > *zeros  and  V1Crmn = *zeros
067500990623     c                   movel     MSG(3)        V1Cmsg
067600990621     c                   eval      *IN43 = *on
067700990621     c                   eval      *IN28 = *on
067800990621      *
067900990621     c                   ENDSL
068000990621      *
068100990621      * Controllo selezione
068200990621     c                   SELECT
068300990621     c                   WHEN      V1Crmn = *zeros
068400990621     c                   WHEN      *IN28 = *ON
068500990621     c                   WHEN      Wselez > *zeros
068600990623     c                   movel     MSG(4)        V1Cmsg
068700990621     c                   eval      *IN42 = *on
068800990621     c                   eval      *IN28 = *on
068900990621     c                   WHEN      V1Cccm > *zeros
069000990621     c                   z-add     3             Wselez
069100990621     c                   OTHER
069200990621     c                   z-add     2             Wselez
069300990621     c                   ENDSL
069400990621      *
069500990623     c                   z-add     V1Cccm        WV1Cccm
069600990623      *
069700990621     c   28              goto      EndCTRD01
069800040216
069900040216      * RIFERIMENTO ALFABETICO MITTENTE
070000040216      *   Controllo selezione
070100040216     c                   SELECT
070200040216     c                   WHEN      V1Crma = *blanks
070300040216     c                   WHEN      Wselez > *zeros
070400040216     c                   movel     MSG(4)        V1Cmsg
070500040216     c                   eval      *IN50 = *on
070600040216     c                   eval      *IN28 = *on
070700040216     c                   OTHER
070800040216     c                   z-add     16            Wselez
070900040216     c                   ENDSL
071000040216     c   28              goto      EndCTRD01
071100990618
071200990621      * RIFERIMENTO PARTNER
071300990621      *   Controllo selezione
071400990621     c                   SELECT
071500990623     c                   WHEN      V1Crpt = *blanks
071600990621     c                   WHEN      Wselez > *zeros
071700990623     c                   movel     MSG(4)        V1Cmsg
071800990621     c                   eval      *IN44 = *on
071900990621     c                   eval      *IN28 = *on
072000990621     c                   OTHER
072100990621     c                   z-add     4             Wselez
072200990621     c                   ENDSL
072300990621     c   28              goto      EndCTRD01
072400000211
072500000211      * NUMERO PARCEL DPD
072600060608     c* deve essere lungo o 11 o 14
072700060609     c                   select
072800060609     c* OK
072900060609     c                   when      v1cdpd=*blanks
073000060609     c                   when      %subst(v1cdpd:14:1)<>' '
073100060609     c* OK
073200060609     c                   when      %subst(v1cdpd:12:3)='   '  and
073300060609     c                             %subst(v1cdpd:11:1)<>' '
073400060609     c
073500060609     c* Se lungo 11 deve essere tutto numerico
073600060609     c                   movea     v1cdpd        w11
073700060609     c                   do        11            xx                3 0
073800060609     c     w11(XX)       lookup    num                                    30
073900060609     c                   if        not *in30
074000060609     c                   eval      *IN52 = *on
074100060609     c                   eval      *IN28 = *on
074200060609     c                   movel     MSG(39)       V1Cmsg
074300060609     c                   leave
074400060609     c                   endif
074500060609     c
074600060609     c                   enddo
074700060609     c
074800060609     c                   other
074900060609     c* parcel dpd errato
075000060609     c                   eval      *IN52 = *on
075100060609     c                   eval      *IN28 = *on
075200060609     c                   movel     MSG(39)       V1Cmsg
075300060609     c                   endsl
075400060609     c   28              goto      EndCTRD01
075500060609     c
075600000211      *   Controllo selezione
075700000211     c                   SELECT
075800060608     c                   WHEN      V1Cdpd = *blanks
075900000211     c                   WHEN      Wselez > *zeros
076000000211     c                   movel     MSG(4)        V1Cmsg
076100000211     c                   eval      *IN52 = *on
076200000211     c                   eval      *IN28 = *on
076300000211     c                   OTHER
076400000211     c                   z-add     10            Wselez
076500000211     c                   ENDSL
076600000211     c   28              goto      EndCTRD01
076700990621
076800990621      * CODICE CLIENTE FATTURAZIONE e DATA FATTURA e NUMERO FATTURA
076900060608     c*  P.O. IVA
077000990621      * Controllo data fattura
077100990621     C                   clear                   WV1Cdft
077200990621     C                   IF        V1Cdft  > *zeros
077300990805     C                   z-add     V1Cdft        WDATA_08
077400990805     C                   exsr      CTR_data
077500990805     C                   IF        *in28 = *on
077600990805     C                   movel     MSG(5)        V1CMSG
077700990805     c                   eval      *in46 = *ON
077800990805     C                   clear                   WV1Cdft
077900990805     C                   ELSE
078000990805     C                   z-add     Wdata_08g     WV1Cdft
078100990805     C                   z-add     Wdata_08      V1Cdft
078200990621     C                   ENDIF
078300990805     C                   ENDIF
078400990805      *
078500990805     c   28              goto      EndCTRD01
078600990621      *
078700990621     C                   SELECT
078800990621      *    se immessa rag. sociale e non il codice cliente richiamo il pgm di ricerca alfabetica
078900990621     c                   WHEN      V1Cksc = *zeros  and  V1Dksc <> *blanks
079000990621     c                   movel(P)  V1Dksc        PAXdmt
079100990621     c                   exsr      ricalf
079200990621     C                   If        PAXsta > -1
079300000901     C                   movel     paxksm        V1Cksc
079400990621     c                   movel(P)  PAXdmt        V1Dksc
079500990621     C                   Endif
079600990621     c                   eval      *IN90 = *on
079700990621      *    se immesso il codice cliente fattura lo decodifico
079800990621     c                   WHEN      V1Cksc > *zeros  and  V1Cksc <> WV1Cksc
079900990621     c                   clear                   V1Dksc
080000990621     c                   move      V1Cksc        w0040
080100990621     c                   IF        W0040 <> 9999  and  W0040 <> 8888
080200990621     c                   z-add     V1Cksc        I69kac
080300990621     c                   exsr      CTR_anagra
080400990707     C                   If         WO69ERR =  '1'
080500990623     c                   movel     MSG(2)        V1Cmsg
080600990621     c                   eval      *IN45 = *on
080700990621     c                   eval      *IN28 = *on
080800990621     c                   Else
080900990621     c                   eval      *IN90 = *on
081000990621     c                   movel     ACOrag        V1Dksc
081100990621     c                   Endif
081200990621     c                   ENDIF
081300060608      *    Se immesso codice cliente non immettere p.o. iva e viceversa     codice fatturazione
081400060608     c                   WHEN      V1Cfiv>0 and v1cksc>0
081500060608     c                   movel     MSG(37)       V1Cmsg
081600060608     c                   eval      *IN45 = *on
081700060608     c                   eval      *IN28 = *on
081800990623      *    non è possibile indicare la data fattura e richiedere le spedizioni non fatturate
081900060608     c                   WHEN      V1Cnfa <> *blanks  and
082000060608     c                             (v1cdft>0 or v1cfiv>0 or v1cnft>0)
082100990623     c                   movel     MSG(8)        V1Cmsg
082200990623     c                   eval      *IN46 = *on
082300990623     c                   eval      *IN28 = *on
082400060608      *    non è possibile indicare il numero fattura senza il codice cliente
082500060608      *    o P.O. iva
082600060608     c                   WHEN      V1Cksc = *zeros  and  V1Cnft > *zeros
082700060608     c                             and v1cfiv=0
082800990623     c                   movel     MSG(10)       V1Cmsg
082900990623     c                   eval      *IN46 = *on
083000990623     c                   eval      *IN28 = *on
083100060608      *    se immesso cliente e numero fattura immettere anche la data      e
083200060608     c                   WHEN      V1Cksc>0  and  V1Cnft > *zero
083300060608     c                             and v1cdft=0
083400060608     c                   movel     MSG(38)       V1Cmsg
083500060608     c                   eval      *IN46 = *on
083600060608     c                   eval      *IN28 = *on
083700990623      *    non è possibile indicare la data fattura senza il codice fatturazione
083800060608     c                   WHEN      v1Cksc = *zeros  and  V1Cdft > *zeros
083900060608     c                             and v1cnft=0 and v1cfiv=0
084000990623     c                   movel     MSG(11)       V1Cmsg
084100990621     c                   eval      *IN45 = *on
084200990621     c                   eval      *IN28 = *on
084300060608      *    Se immesso p.o. IVA immettere anche numero fattura               codice fatturazione
084400060608     c                   WHEN      V1Cfiv>0 and v1cnft=0
084500060608     c                   movel     MSG(36)       V1Cmsg
084600060608     c                   eval      *IN57 = *on
084700060608     c                   eval      *IN28 = *on
084800990623      *    non è possibile selezionare le spedizioni non fatturate senza il codice fatturazione
084900990623     c                   WHEN      V1Cksc = *zeros  and  V1Cnfa <> *blanks
085000990623     c                   movel     MSG(12)       V1Cmsg
085100990623     c                   eval      *IN45 = *on
085200990623     c                   eval      *IN28 = *on
085300990621      *
085400990621     c                   ENDSL
085500990621      *
085600990621      * Controllo selezione
085700990621     c                   SELECT
085800060608     c                   WHEN      V1Cksc = *zeros  and v1cfiv=0
085900990621     c                   WHEN      *IN28 = *ON
086000990621     c                   WHEN      Wselez > *zeros
086100990623     c                   movel     MSG(4)        V1Cmsg
086200990621     c                   eval      *IN45 = *on
086300990621     c                   eval      *IN28 = *on
086400060608     c                   WHEN      v1cfiv>0
086500060608     c                   z-add     17            Wselez
086600060608     c                   WHEN      v1cksc>0 and V1Cnft > *zeros
086700060608     c                   z-add     7             Wselez
086800060608     c                   WHEN      V1Cdft > *zeros  or  V1Cnfa <> *blanks
086900990621     c                   z-add     6             Wselez
087000990621     c                   OTHER
087100990621     c                   z-add     5             Wselez
087200990621     c                   ENDSL
087300990623      *
087400990623     c                   z-add     V1Cksc        WV1Cksc
087500990623      *
087600990621     c   28              goto      EndCTRD01
087700990618
087800990621      * DATA SPEDIZIONE DAL/AL
087900990621     C                   clear                   WV1Cdsd
088000990622     C                   clear                   WV1Cdsa
088100990623      *     spedizione al
088200990623     C                   IF        V1Cdsa  > *zeros
088300990805     C                   z-add     V1Cdsa        WDATA_08
088400990805     C                   exsr      CTR_data
088500990805     C                   IF        *in28 = *on
088600990805     c                   eval      *in48 = *ON
088700990805     C                   ELSE
088800990805     C                   z-add     Wdata_08g     WV1Cdsa
088900990805     C                   z-add     Wdata_08      V1Cdsa
089000990805     C                   ENDIF
089100990623     C                   ENDIF
089200990623      *     spedizione dal
089300110110     C                   IF        V1Cdsd  > *zeros and not *in28
089400990805     C                   z-add     V1Cdsd        WDATA_08
089500990805     C                   exsr      CTR_data
089600990805     C                   IF        *in28 = *on
089700990805     c                   eval      *IN47 = *on
089800990805     c                   eval      *IN48 = *off
089900990805     C                   ELSE
090000990805     C                   z-add     Wdata_08g     WV1Cdsd
090100990805     C                   z-add     Wdata_08      V1Cdsd
090200990805     C                   ENDIF
090300990621     C                   ENDIF
090400990622      *
090500990622      * Gestione errori, controllo congruità date e Controllo selezione
090600990621     c                   SELECT
090700990623     c                   WHEN      *IN47 = *ON  or  *IN48 = *ON
090800990623     c                   movel     MSG(13)       V1Cmsg
090900990623     c                   eval      *IN28 = *on
091000990622      *
091100990624     c                   WHEN      WV1Cdsa < WV1Cdsd  and  WV1Cdsa <> *zeros
091200990623     c                   movel     MSG(14)       V1Cmsg
091300990621     c                   eval      *IN28 = *on
091400990621     c                   eval      *IN47 = *on
091500010112      **!!!****
091600010112      *   obbligatoria la data se selezione 13 e segnacollo non univoco
091700010112     c                   WHEN      wselez = 13 and  V1flgs = *blanks
091800010112     C                             and v1cfls > *zeros and v1cdsd = *zeros
091900010112     c                   movel     MSG(27)       V1Cmsg
092000010112     c                   eval      *IN28 = *on
092100010112     c                   eval      *IN47 = *on
092200010112     c                   goto      EndCTRD01
092300010112      **!!!****
092400990621      *
092500990621     c                   WHEN      V1Cdsd = *zeros
092600000714      *
092700000714      *              obbligatorio P.O. partenza
092800000714     c                   WHEN      wselez = *zeros  and  V1Clp1 = *zeros
092900000714     c                   movel     MSG(25)       V1Cmsg
093000000714     c                   eval      *IN28 = *on
093100000714     c                   eval      *IN49 = *on
093200010112
093300000714      *              se selezionato il codice fatturazione (sel. 5) vince data spedizione
093400000714      ******** ! >>> se si disasterisca ATTENZIONE xchè è stata aggiunta l'obbligatorietà
093500000714      ******** ! >>> del P.O. di partenza ed è stata cambiata la routine per selezione = 8
093600000714     c******** ! >>>     WHEN      Wselez = 5
093700000714     c******** ! >>>     z-add     8             Wselez
093800000714      *
093900991103     c                   WHEN      Wselez = *zeros
094000991103     c                   z-add     8             Wselez
094100991103      *
094200990813      *
094300990621     c                   ENDSL
094400990622      *
094500990622     C                   If        WV1Cdsa = *zeros
094600990622     c                   z-add     WV1Cdsd       WV1Cdsa
094700990622     C                   Endif
094800990621     c   28              goto      EndCTRD01
094900000718      *
095000000718      * max mese successivo
095100000718     C     Wv1cdsd       ifgt      0
095200000718     C                   movel     Wv1cdsd       WorkAnno          4 0
095300000718     C                   move      Wv1cdsd       Workmesegg        4 0
095400000718     C                   move      '31'          Workmesegg
095500000718     C     Workmesegg    ifeq      1231
095600000718     C                   z-add     131           Workmesegg
095700000718     C                   add       1             Workanno
095800000718     C                   else
095900000718     C                   add       100           Workmesegg
096000000718     C                   endif
096100000718     C                   movel     workanno      workdata          8 0
096200000718     C                   move      workmesegg    workdata
096300000718     C     wv1cdsa       ifgt      workdata
096400000718     c                   movel     MSG(29)       V1Cmsg
096500000718     C                   seton                                        28  48
096600000718     C                   endif
096700000718     C                   endif
096800000718     c   28              goto      EndCTRD01
096900040216
097000040216      * CODICE BOLLA
097100040216if  1c                   if        V1Ccbo <> *blanks
097200040216     c                   movel     CodUt         TBLkut
097300040216     c                   movel     '3A'          TBLcod
097400040216      *
097500040216     c                   eval      *in30  =  *off
097600040216     c     '?'           scan      V1Ccbo                                 30
097700040216if  2c                   if        *in30 = *on
097800040216     c                   call      'X§TABER'
097900040216     c                   parm                    TBLkut
098000040216     c                   parm                    TBLcod
098100040216     c                   parm                    TBLkey
098200040216     c                   parm                    §DES             30
098300040216     c                   movel     TBLkey        V1Ccbo
098400040216     c                   eval      *in90 = *on
098500040216e   2c                   endif
098600040216      *
098700040216     c                   movel(p)  V1Ccbo        TBLkey
098800040216     c     K03TAB00      chain     TABEL
098900040216if  2c                   if        NOT %found(TABEL00F)
099000040216     c                             or  TBLflg <> *blanks
099100040216     c                   movel     Msg(35)       V1Cmsg
099200040216     c                   seton                                        285690
099300040216     c                   goto      EndCtrD01
099400040216e   2c                   endif
099500040216e   1c                   endif
099600990621
099700000714      * DESTINATARIO
099800000714     C     Wselez        ifeq      *zeros
099900000714     C     V1Crsd        andne     *blanks
100000000714     C                   z-add     11            Wselez
100100000714     C     V1Clp1        ifeq      *zeros
100200000714     c                   eval      *IN28 = *on
100300000714     c                   eval      *IN49 = *on
100400000714     c                   movel     MSG(25)       V1Cmsg
100500000714     C                   endif
100600000714     C                   endif
100700000714     c   28              goto      EndCTRD01
100800000714
100900000714      * P.O. PARTENZA
101000990621     c                   clear                   V1Dlp1
101100990621     C                   IF        V1Clp1 > *zeros
101200990623     C                   z-add     V1Clp1        Wlinea
101300990621     c                   exsr      CTR_linea
101400990621     c                   If        *IN28 = *ON
101500990621     c                   eval        *IN49 = *on
101600990623     c                   movel     MSG(7)        V1Cmsg
101700990621     c                   Else
101800990621     C                   movel     ORGDES        V1Dlp1
101900000717     C     Wselez        ifeq      0
102000000717     c                   movel     MSG(27)       V1Cmsg
102100000718     C                   seton                                        47  28
102200000717     C                   endif
102300990621     C                   Endif
102400990621     C                   ENDIF
102500990621     c   28              goto      EndCTRD01
102600000714
102700000714      * SERIE
102800000714     C                   setoff                                       35  36
102900000714     C     v1cser        ifne      '  '
103000010129     C                   if        v1cnrs <> *zeros
103100010129     C                             or v1cnrr <> *zeros
103200010129     C                   movel     MSG(26)       V1Cmsg
103300010129     C                   eval      *IN53 = *on
103400010129     C                   eval      *IN28 = *on
103500010129     C                   goto      EndCTRD01
103600010129     C                   endif
103700000714     C                   movel     v1cser        wserie1           1
103800000714     C                   move      v1cser        wserie2           1
103900000714     C     wserie2       ifeq      ' '
104000000714     C                   eval      wserie2 = wserie1
104100000714     C                   eval      wserie1 = ' '
104200000714     C                   movel     wserie1       v1cser
104300000714     C                   move      wserie2       v1cser
104400000714     C                   endif
104500000714     C     wserie1       ifeq      ' '
104600000714     C                   eval      wserie1 = '0'
104700000714     C                   endif
104800000714      *
104900000714     C     wserie1       ifeq      '0'
105000000714     C                   seton                                        35
105100000714     C                   endif
105200000714     C  N351             do        9             sr                2 0
105300000714     C                   move      sr            wnumalfa          1
105400000714     C     wserie1       ifeq      wnumalfa
105500000714     C                   seton                                        35
105600000714     C                   endif
105700000714     C                   enddo
105800000714      *
105900000714     C     wserie2       ifeq      '0'
106000000714     C                   seton                                        36
106100000714     C                   endif
106200000714     C  N361             do        9             sr
106300000714     C                   move      sr            wnumalfa          1
106400000714     C     wserie2       ifeq      wnumalfa
106500000714     C                   seton                                        36
106600000714     C                   endif
106700000714     C                   enddo
106800000714      *
106900000714     C     *in35         ifeq      *off
107000000714     C     *in36         oreq      *off
107100000714     c                   movel     MSG(24)       V1Cmsg
107200000714     c                   eval      *IN53 = *on
107300000714     c                   eval      *IN28 = *on
107400000717     C                   else
107500000717     C     Wselez        ifeq      8
107600000717     C                   z-add     12            Wselez
107700000717     C                   endif
107800000714     C                   endif
107900000714     C                   endif
108000000714     c   28              goto      EndCTRD01
108100990621
108200990621      * P.O. ARRIVO
108300990621     c                   clear                   V1Dlna
108400990621     C                   IF        V1Clna > *zeros
108500990623     C                   z-add     V1Clna        Wlinea
108600990621     c                   exsr      CTR_linea
108700990621     c                   If        *IN28 = *ON
108800010104     c                   eval        *IN54 = *on
108900990623     c                   movel     MSG(7)        V1Cmsg
109000990621     c                   Else
109100990621     C                   movel     ORGDES        V1Dlna
109200990621     C                   Endif
109300990621     C                   ENDIF
109400990621     c   28              goto      EndCTRD01
109500990813
109600990813      * CONTROLLO SELEZIONE VALIDA
109700990813     c                   IF        Wselez = *zeros
109800990813     c                   movel     MSG(15)       V1Cmsg
109900990813     c                   eval      *IN40 = *on
110000990813     c                   eval      *IN28 = *on
110100990813     c                   ENDIF
110200990813     c   28              goto      EndCTRD01
110300990621
110400990618     c     Endctrd01     ENDSR
110500990618      *****************************************************************
110600990618      * CARICAMENTO SUBFILE VIDEO 2
110700990618      *****************************************************************
110800990618     C     RIED02        BEGSR
110900990618      *
111000990618      * Pulisco subfile
111100990618     c                   IF        *IN23 = *off
111200990618     C                   eval         *IN21 = *OFF
111300990622     C                   WRITE     SB50C02
111400990618     C                   eval         *IN20 = *ON
111500990618     C                   eval         *IN21 = *ON
111600990618     c                   ENDIF
111700990618      *
111800990618     c                   clear                   Wendpag
111900990623     c                   clear                   Wspedpag
112000990623     c                   z-add     NUMspedcar    NRR1
112100990622      *
112200990618     c                   SELECT
112300990622      * spedizione
112400990622     C                   WHEN      Wselez = 1
112500990618     C                   exsr      RIESEL1
112600040216      * riferimento mittente numerico
112700990622     C                   WHEN      Wselez = 2
112800990622     C                   exsr      RIESEL2
112900040216      * riferimento mittente numerico e codice mittente
113000990622     C                   WHEN      Wselez = 3
113100990622     C                   exsr      RIESEL3
113200990622      * riferimento partner
113300990622     C                   WHEN      Wselez = 4
113400990622     C                   exsr      RIESEL4
113500990622      * codice cliente fatturazione e (data fattura o non fatturate)
113600990707     C                   WHEN      Wselez = 5  or  Wselez = 6
113700990707     C                   exsr      RIESEL56
113800990622      * codice cliente fatturazione e data fattura e numero fattura
113900990622     C                   WHEN      Wselez = 7
114000990622     C                   exsr      RIESEL7
114100000717      * data spedizione (e lnp)
114200990622     C                   WHEN      Wselez = 8
114300990622     C                   exsr      RIESEL8
114400990813      * lnp e data spedizione
114500991103     C*                  WHEN      Wselez = 9
114600991103     C*                  exsr      RIESEL9
114700000211      * numero parcel DPD
114800000211     C                   WHEN      Wselez = 10
114900000222     C                   exsr      M1031
115000000222     C                   exsr      RIESEL10
115100000717      * lnp e destinatario
115200000717     C                   WHEN      Wselez = 11
115300000717     C                   exsr      RIESEL11
115400000717      * anno + lnp + serie
115500000717     C                   WHEN      Wselez = 12
115600000717     C                   exsr      RIESEL12
115700000717      * secco per segnacollo su TITAT27C
115800000717     C                   WHEN      Wselez = 13
115900000717     C                   exsr      RIESEL13
116000000901      * codice "CHI SONO"
116100000901     C                   WHEN      Wselez = 14
116200000901     C                   exsr      RIESEL14
116300010219      * numero ORM
116400010219     C                   WHEN      Wselez = 15
116500010219     C                   exsr      RIESEL15
116600040216      * riferimento mittente alfabetico
116700040216     C                   WHEN      Wselez = 16
116800040216     C                   exsr      RIESEL16
116900060608     c
117000060608     c* P.O. iva e numero fattura
117100060608     C                   WHEN      Wselez = 17
117200060608     C                   exsr      RIESEL17
117300990618      *
117400990618     C                   ENDSL
117500990618      *
117600990618     c                   z-add     1             V2Cnrr
117700990623     c                   z-add     NRR1          NUMspedcar
117800990618      *
117900990618     c                   ENDSR
118000990707      *****************************************************************
118100990707      * CARICAMENTO SUBFILE * SELEZIONE 1 - spedizione
118200990707      *****************************************************************
118300990707     C     RIESEL1       BEGSR
118400990707      *
118500990707      * Se il chiamante ha passato la key sped. con il tipo bolla chaino con chiave completa
118600990707     c                   IF        D50tbl <> *blanks
118700990707     c                             and  %SUBST(I50OP0:2:2) <> '05'
118800990707     C     kspe1         chain     TITAS30C                           24
118900990707     C  N24              exsr      RICsbfl
119000990707     C                   ELSE
119100990707      *
119200990707     c                   IF        *IN23 = *OFF
119300990707     C     kspe          chain     TITAS30C                           24
119400990707     c                   ENDIF
119500990707      *
119600990707     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
119700030529     C*
119800030529     C* Inserisco qui i controlli x verificare la data limite x considerare le
119900030529     C* bolle PT (tale controllo nn viene effettuato se l'utente è del gruppo
120000030529     C* EDP o ISP).
120100030529     C                   if        %subst(KPJBU:2:2) <> '05'
120200030529     C                   if        %subst(KNMUS:1:3) = 'EDP'
120300030529     C                   else
120400030529     C     tasLNP        lookup    FILPOSTE                               96
120500030529     C                   if        *in96
120600030529     C                   if        tasDCM >  0
120700030529     C                   if        tasDCM >= wDataLimitePT
120800030529     C                   else
120900030529     C                   seton                                        24
121000030529     C                   endif
121100030529     C                   endif
121200030529     C                   endif
121300030529     C                   endif
121400030529     C                   endif
121500030529     C*
121600030529     C                   if        *in24 = *off
121700990707     C                   exsr      RICsbfl
121800990707     C     kspe          reade     TITAS30C                               24
121900030529     C                   endif
122000990707     C                   ENDDO
122100990707      *
122200990707     c                   ENDIF
122300990707      *
122400990707      *  Se non ho trovato record aggancio il file delle bolle annullate
122500990707     c                   IF        NUMspedcar = *zeros
122600110502     C     kspe          chain     TNBLA01L                           24
122700110502     c                   If        *in24 = *off
122800110502     c                   movel     msg(19)       V1Cmsg
122900110502     c                   Endif
123000990707     c                   ENDIF
123100990707      *
123200990707     c                   ENDSR
123300990707      *****************************************************************
123400990707      * CARICAMENTO SUBFILE * SELEZIONE 2 - Rif. mittente
123500990707      *****************************************************************
123600990707     C     RIESEL2       BEGSR
123700990707      *
123800990707     c                   IF        *IN23 = *OFF
123900990707     C     V1Crmn        chain     TITAS32C                           24
124000990707     c                   ENDIF
124100990707      *
124200000211     c                   DOW       *in24=*Off  and  Wendpag <> 'S'
124300990707     C                   exsr      CTR_selez
124400990707     C     V1Crmn        reade     TITAS32C                               24
124500990707     C                   ENDDO
124600990707      *
124700990707     c                   ENDSR
124800990707      *****************************************************************
124900990707      * CARICAMENTO SUBFILE * SELEZIONE 3 - Rif. mittente + codice
125000990707      *****************************************************************
125100990707     C     RIESEL3       BEGSR
125200990707      *
125300990707     c                   IF        *IN23 = *OFF
125400990707     C     Krmn          chain     TITAS32C                           24
125500990707     c                   ENDIF
125600990707      *
125700990707     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
125800990707     C                   exsr      CTR_selez
125900990707     C     Krmn          reade     TITAS32C                               24
126000990707     C                   ENDDO
126100990707      *
126200990707     c                   ENDSR
126300990707      *****************************************************************
126400990707      * CARICAMENTO SUBFILE * SELEZIONE 4 - Rif. partner
126500990707      *****************************************************************
126600990707     C     RIESEL4       BEGSR
126700990707      *
126800070927     c***                movel(P)  V1Crpt        W020A
126900070927     c                   movel(P)  V1Crpt        W030A
127000990707      *
127100990707     c                   IF        *IN23 = *OFF
127200070927     C     W030A         chain     TITA438C                           33
127300000901     C  N33Ktas          chain     TITAS30C                           24
127400990707     c                   ENDIF
127500990707      *
127600000901     c                   DOW       *IN33 = *off
127700000901     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
127800990707     C                   exsr      CTR_selez
127900040217     c     Wendpag       cabeq     'S'           EndSel04
128000000901     C     Ktas          reade     TITAS30C                               24
128100990707     C                   ENDDO
128200040217     c     Wendpag       cabeq     'S'           EndSel04
128300070927     C     W030A         reade     TITA438C                               33
128400000901     C  N33Ktas          chain     TITAS30C                           24
128500000901     C                   ENDDO
128600990707      *
128700040217     c     EndSel04      ENDSR
128800990622      *****************************************************************
128900990707      * CARICAMENTO SUBFILE * SELEZIONE 5/6 - Cliente fatturazione e data fattura/non fatturate
129000990622      *****************************************************************
129100990707     C     RIESEL56      BEGSR
129200990622      *
129300990622     c                   IF        *IN23 = *OFF
129400990707     C                   IF        Wselez = 5
129500990707     C     Kfat1         setll     TITAS31C
129600990707     c                   ELSE
129700990707     C     kfat2         setll     TITAS31C
129800990622     c                   ENDIF
129900990825     c
130000990825     C                   IF        Wselez = 5
130100990707     C     KFAT1         reade     TITAS31C                               24
130200990825     c                   else
130300990825     C     kfat2         reade     TITAS31C                               24
130400990825     c                   endif
130500990707     c                   ENDIF
130600990622      *
130700990707     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
130800990622     C                   exsr      CTR_selez
130900990825     C                   IF        Wselez = 5
131000990707     C     KFAT1         reade     TITAS31C                               24
131100990825     c                   else
131200990825     C     kfat2         reade     TITAS31C                               24
131300990825     c                   endif
131400990622     C                   ENDDO
131500990622      *
131600990622     c                   ENDSR
131700990707      *****************************************************************
131800990707      * CARICAMENTO SUBFILE * SELEZIONE 7 - Codice cliente fatturazione e data fattura e n. fattura
131900990707      *****************************************************************
132000990707     C     RIESEL7       BEGSR
132100990707      *
132200990707     c                   IF        *IN23 = *OFF
132300990716     C     KFAT3         chain     TITAS31C                           24
132400990707     c                   ENDIF
132500990707      *
132600990707     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
132700990707     C                   exsr      CTR_selez
132800990716     C     KFAT3         reade     TITAS31C                               24
132900990707     C                   ENDDO
133000990707      *
133100990707     c                   ENDSR
133200060608      *****************************************************************
133300060608      * CARICAMENTO SUBFILE * SELEZIONE 17 - P.O, iva e numero fattura      ta fattura e n. fattura
133400060608      *****************************************************************
133500060608     C     RIESEL17      BEGSR
133600060608      *
133700060608     c                   IF        *IN23 = *OFF
133800060608     c                   if        v1cdft=0
133900060608     C     KFAT4         chain     TITAS40C                           24
134000060608     c                   else
134100060608     C     KFAT43        chain     TITAS40C                           24
134200060608     c                   endif
134300060608     c
134400060608     c                   ENDIF
134500060608      *
134600060608     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
134700060608     C                   exsr      CTR_selez
134800060608     c                   if        v1cdft=0
134900060608     C     KFAT4         reade     TITAS40C                               24
135000060608     c                   else
135100060608     C     KFAT43        reade     TITAS40C                               24
135200060608     c                   endif
135300060608     C                   ENDDO
135400060608      *
135500060608     c                   ENDSR
135600990707      *****************************************************************
135700990707      * CARICAMENTO SUBFILE * SELEZIONE 8 - data spedizione
135800990707      *****************************************************************
135900990707     C     RIESEL8       BEGSR
136000000718      *
136100990707     c                   IF        *IN23 = *OFF
136200990707     c                   movel     WV1Cdsd       Kaas
136300990707     c                   move      WV1Cdsd       Kmgs
136400010112     C                   eval      klinea = v1clp1
136500990707      *
136600000717     C     kdta          setll     TITAS39C
136700000717     C     v1cLP1        reade     TITAS39C                               24
136800990707     c                   ENDIF
136900990707      *
137000990707     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
137100990707     C                   exsr      CTR_selez
137200990707      * Se supero data limite fine lettura
137300990707     C                   If        Wdatasp > WV1Cdsa
137400990707     C                   eval      *IN24 = *on
137500990707     c                   Else
137600000717     C     v1cLP1        reade     TITAS39C                               24
137700990707     c                   Endif
137800990707     C                   ENDDO
137900000718      *
138000000718     c     endsbr8       ENDSR
138100990813      *****************************************************************
138200990813      * CARICAMENTO SUBFILE * SELEZIONE 9 - LNP e data spedizione
138300990813      *****************************************************************
138400991103     C*    RIESEL9       BEGSR
138500990813      *
138600991103     c*                  IF        *IN23 = *OFF
138700991103     C*    kspe9         setll     TITAS30C
138800991103     C*    Kspe9         reade     TITAS30C                               24
138900991103     c*                  ENDIF
139000990813      *
139100991103     c*                  DOW       *IN24 = *off  and  Wendpag <> 'S'
139200991103     C*                  exsr      CTR_selez
139300990813      * Se supero data limite fine lettura
139400991103     C*                  If        Wdatasp > WV1Cdsa
139500991103     C*                  eval      *IN24 = *on
139600991103     c*                  Else
139700991103     C*    Kspe9         reade     TITAS30C                               24
139800991103     c*                  Endif
139900991103     C*                  ENDDO
140000990813      *
140100991103     c*                  ENDSR
140200000222      * ------------------------------------------------------------------*
140300000222      *  CALCOLA CHECK DIGIT PER RIFERIMENTO DPD
140400000222      * ------------------------------------------------------------------*
140500000222     C     M1031         BEGSR
140600060607     c                   clear                   w0150
140700060607     c
140800060607     c                   move      v1cdpd        w003a             3
140900060607     c                   if        w003a=*blanks
141000000222      *
141100060609     C                   MOVEL(P)  v1cdpd        DPDBRC
141200000222      *
141300000222     c                   clear                   W0030a
141400000222     c                   clear                   w0030
141500000222      *
141600000222      * TOTALIZZO I VALORI, QUELLI NELLE POSIZIONI DISPARI PER 3
141700000222     c                   DO        11            xx
141800000222      *
141900000222     c     xx            div       2             W0030a
142000000222     c                   mvr                     Wresto
142100000222     c                   If        Wresto > *zeros
142200000222     c                   eval       W0030 = W0030 + (§11(xx) * 3)
142300000222     c                   Else
142400000222     c                   add       §11(xx)       W0030
142500000222     c                   Endif
142600000222      *
142700000222     c                   ENDDO
142800000222      *
142900000222     c     w0030         add       9             W0030a
143000000222     c                   move      0             W0030a
143100000222     c     W0030a        sub       w0030         dpdchd
143200000222     c
143300000222     C                   Z-ADD     DPDBRC        W0150
143400060607     c                   endif
143500000222      *
143600000222     c                   endsr
143700000211      *****************************************************************
143800000211      * CARICAMENTO SUBFILE * SELEZIONE 10 - Numero Parcel DPD
143900000211      *****************************************************************
144000000211     C     RIESEL10      BEGSR
144100000211      *
144200060607      * Ricerco sia import che export Export in TITA4
144300061120     c                   if        wgiro<>'2'
144400060529     c                   movel(p)  V1Cdpd        W014A
144500061120     c                   endif
144600000211      *
144700000211     c                   IF        *IN23 = *OFF
144800060529     C     W014A         chain     TITA437C                           33
144900000901     C  N33Ktas          chain     TITAS30C                           24
145000000211     c                   ENDIF
145100000211      *
145200061120     c     rsel10_tag    tag
145300061120     c*
145400000901     c                   DOW       *IN33 = *off
145500000901     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
145600000223     C                   exsr      ctrspeddpd
145700000223     C  N31              exsr      CTR_selez
145800040217     c     Wendpag       cabeq     'S'           EndSel10
145900000901     C     Ktas          reade     TITAS30C                               24
146000000901     C                   ENDDO
146100040217     c     Wendpag       cabeq     'S'           EndSel10
146200060529     C     W014A         reade     TITA437C                               33
146300000901     C  N33Ktas          chain     TITAS30C                           24
146400000901     C                   ENDDO
146500000211      *
146600060607      * Ricerco Import nel Rif mittente numerico  solo se immesso
146700060607      *  un numero di 11 cifre (visto che il new sarà alfa e lungo 14)
146800061120     c                   if        w0150>0 and wgiro<>'2'
146900060607     c                   IF        *IN23 = *OFF
147000060607     C     W0150         chain     TITAS32C                           24
147100060607     c                   ENDIF
147200000211      *
147300060607     c                   DOW       *in24=*Off  and  Wendpag <> 'S'
147400060607     C                   exsr      ctrspeddpd
147500060607     C  N31              exsr      CTR_selez
147600060607     C     W0150         reade     TITAS32C                               24
147700060607     C                   ENDDO
147800061120     c*
147900061120     c                   eval      w014a='0'+ %subst(v1cdpd:1:3)+'99'+
148000061120     c                             %subst(v1cdpd:4:8)
148100061120     C     W014A         chain     TITA437C                           33
148200061120     C  N33Ktas          chain     TITAS30C                           24
148300061120     c                   eval      wgiro='2'
148400061120     c                   goto      rsel10_tag
148500060607     c
148600060607     c                   ENDIF
148700000211      *
148800040217     c     EndSel10      ENDSR
148900000717      *****************************************************************
149000000717      * CARICAMENTO SUBFILE * SELEZIONE 11 - lnp + destinatario
149100000717      *****************************************************************
149200000717     C     RIESEL11      BEGSR
149300000717      *
149400000717     c                   IF        *IN23 = *OFF
149500000717      *
149600000717     C     kspe11        setll     TITAS37C
149700000717     C     V1Clp1        reade     TITAS37C                               24
149800000717     c                   ENDIF
149900000717      *
150000000717     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
150100000717     C                   exsr      CTR_selez
150200000717      * esco per destinatario non trovato
150300000717     c                   If        %SUBST(V1CRSD:1:XX) <> %SUBST(TASRSD:1:XX)
150400000717     C                   eval      *IN24 = *on
150500000717     c                   Else
150600000717     C     V1Clp1        reade     TITAS37C                               24
150700000717     c                   Endif
150800000717     C                   ENDDO
150900000717      *
151000000717     c                   ENDSR
151100000717      *****************************************************************
151200000718      * CARICAMENTO SUBFILE * SELEZIONE 12 - lnp + data + serie
151300000717      *****************************************************************
151400000717     C     RIESEL12      BEGSR
151500000718      *
151600000718     c                   IF        *IN23 = *OFF
151700000718     c                   movel     WV1Cdsd       Kaas
151800000718     c                   move      WV1Cdsd       Kmgs
151900000718     C                   move      V1Cser        WorkSerie
152000010112     C                   eval      klinea = v1clp1
152100000718     C     leggititas1   tag
152200000718     C     kdta1         setll     TITAS39C
152300000718     C     kdta1         reade     TITAS39C                               24
152400000718     C     *in24         ifeq      *on
152500000718     C     kdta          setgt     titas39c
152600000718     C     V1Clp1        reade     titas39c                               24
152700000718     C   24              goto      endsbr12
152800000718     c                   eval      Kaas = TASaas
152900000718     c                   eval      Kmgs = TASmgs
153000000718     C                   goto      leggititas1
153100000718     C                   endif
153200000718     C                   ENDIF
153300000718      *
153400000718     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
153500000718     C                   exsr      CTR_selez
153600000718     C     wdatasp       ifle      WV1Cdsa
153700000718     C     leggititas2   tag
153800000718     C     Kdta1         reade     TITAS39C                               24
153900000718     C     *in24         ifeq      *on
154000000718     C     kdta          setgt     titas39c
154100000718     C     V1Clp1        reade     titas39c                               24
154200000718     C   24              goto      endsbr12
154300000718     c                   eval      Kaas = TASaas
154400000718     c                   eval      Kmgs = TASmgs
154500000718     C     kdta1         setll     TITAS39C
154600000718     C                   goto      leggititas2
154700000718     C                   endif
154800000718     C                   else
154900000718     C                   seton                                        24
155000000718     C                   endif
155100000718     C                   ENDDO
155200000718      *
155300000718     c     endsbr12      ENDSR
155400000717      *****************************************************************
155500010104      * CARICAMENTO SUBFILE * SELEZIONE 13 - segnacollo
155600000717      *****************************************************************
155700000717     C     RIESEL13      BEGSR
155800010111
155900010112     c                   clear                   bolle
156000010112     C                   eval      *in15 = (v1clna > *zeros)
156100010112
156200010129      * Segnacollo univoco (segnacollo da TITAS o presente in TITAT)
156300010112     C                   if        v1flgs <> *blanks
156400010111
156500010109      * Lettura su TITAS (segnacolli sequenziali)
156600010112     C   15Ktat          setll     titas34c
156700010112     C  n15ktat1         setll     titas34c
156800010111     C                   do        *hival
156900010112     C   15Ktat          reade     titas34c
157000010112     C  n15ktat1         reade     titas34c
157100010111
157200010111     C                   if        %eof(titas34c) or wendpag = 'S'
157300010111     C                   leave
157400010111     C                   endif
157500010112      * Selezioni
157600010104     C                   exsr      CTR_selez
157700010111
157800010129      * Carico una schiera con le bolle che trovo
157900010129     C                   if        *in32 = *on
158000010111     c     dsbolle       lookup    bolle                                  30
158100010111     c                   IF        *IN30 = *off
158200010111     c                   eval      xb = 1
158300010111     c     *blanks       lookup    bolle(xb)                              30
158400010111     c   30              eval      bolle(xb) = dsbolle
158500010111     c                   ENDIF
158600010112     C                   endif
158700010111
158800010111     C                   enddo
158900010112
159000010129      * Lettura su TITAT
159100010129      * segnacolli non sequenziali o sequenziali con data consegna <> da TAS
159200010112     c   15ktat          setll     titat24c
159300010112     c  n15ktat1         setll     titat24c
159400010109     C                   do        *hival
159500010112     C   15ktat          reade     titat24c
159600010112     C  n15ktat1         reade     titat24c
159700010112     C                   if        %eof(titat24c) or Wendpag = 'S'
159800010109     C                   leave
159900010109     C                   endif
160000010112     C     ktats         chain     titas30c
160100010109     C                   if        %found(titas30c)
160200010112      * selezioni
160300010111     C                   exsr      CTR_selez
160400010111
160500010109      * Controllo se ho già scritto la bolla
160600010112     c   32dsbolle       lookup    bolle                                  30
160700010112     C   30              iter
160800010109
160900010112     C                   endif
161000010129     C                   enddo
161100010111
161200010111     C                   endif
161300010111
161400010129      * Segnacollo non univoco
161500010129      * Sequenziali: compreso tra segnac. da e segnac. a in TITAS
161600010129      * Non Sequenziali: presente in TITAT
161700010112     C                   if        v1flgs = *blanks
161800010111
161900010111      * Lettura su TITAS
162000010115     c                   IF        *IN23 = *OFF
162100010112     c                   movel     WV1cdsd       Kaas
162200010112     c                   move      WV1cdsd       Kmgs
162300010112     C                   move      V1Cnrr        WorkSerie
162400010112     C                   move      V1Cnsc        KSEGNA
162500010112     C                   eval      klinea = v1cfls
162600010112     C     leggitita     tag
162700010112     C     kdta2         setgt     TITAS35C
162800010112     C     kdta1         readpe    TITAS35C                               24
162900010111     C     *in24         ifeq      *on
163000010112     C     kdta          setgt     titas35c
163100010112     C     V1Cfls        reade     titas35c                               24
163200010115     C   24              goto      leggititat
163300010111     c                   eval      Kaas = TASaas
163400010111     c                   eval      Kmgs = TASmgs
163500010112     C                   goto      leggitita
163600010111     C                   endif
163700010115     C                   endif
163800010111      *
163900010111     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
164000010115     C                   if        tasncd > *zeros
164100010111     C                   exsr      CTR_selez
164200010129      * Carico una schiera con le bolle che trovo.
164300010129      * Solo serie > 0 perchè elaboro TITAT
164400010112     C                   if        *in32 = *on  and v1cnrr > *zeros
164500010112     c     dsbolle       lookup    bolle                                  30
164600010112     c                   IF        *IN30 = *off
164700010112     c                   eval      xb = 1
164800010112     c     *blanks       lookup    bolle(xb)                              30
164900010112     c   30              eval      bolle(xb) = dsbolle
165000010112     c                   ENDIF
165100010112     C                   endif
165200010115     C                   endif
165300010112
165400010112     C     wdatasp       ifle      WV1cdsa
165500010112     C     leggitita1    tag
165600010112     C     Kdta1         readpe    TITAS35C                               24
165700010111     C     *in24         ifeq      *on
165800010112     C     kdta          setgt     titas35c
165900010112     C     V1Cfls        reade     titas35c                               24
166000010115     C   24              goto      leggititat
166100010111     c                   eval      Kaas = TASaas
166200010111     c                   eval      Kmgs = TASmgs
166300010112     C     kdta2         setgt     TITAS35C
166400010112     C                   goto      leggitita1
166500010111     C                   endif
166600010111     C                   else
166700010111     C                   seton                                        24
166800010111     C                   endif
166900010111     C                   ENDDO
167000010112
167100010115     C     leggititat    tag
167200010112      * Lettura su TITAT
167300010129      * Per semplicità di pgm leggo anche eventuali segnac. sequenziali con
167400010129      * serie e data cons. <> da bolla che ho gia elaborato prima.
167500010112     C                   if        v1cnrr > *zeros
167600010112     c   15ktat          setll     titat24c
167700010112     c  n15ktat1         setll     titat24c
167800010112     C                   do        *hival
167900010112     C   15ktat          reade     titat24c
168000010112     C  n15ktat1         reade     titat24c
168100010112     C                   if        %eof(titat24c) or Wendpag = 'S'
168200010112     C                   leave
168300010112     C                   endif
168400010112     C     ktats         chain     titas30c
168500010112     C                   if        %found(titas30c)
168600010112      * selezioni
168700010112     C                   exsr      CTR_selez
168800010112      * Controllo se ho già scritto la bolla
168900010112     c   32dsbolle       lookup    bolle                                  30
169000010112     C   30              iter
169100010112
169200010112     C                   endif
169300010112     C                   enddo
169400010112     C                   endif
169500010111
169600010111     C                   endif
169700010111
169800000717      *
169900010115     c                   ENDSR
170000000901      *****************************************************************
170100000901      * CARICAMENTO SUBFILE * SELEZIONE 14 - Codice "CHI SONO"
170200000901      *****************************************************************
170300000901     C     RIESEL14      BEGSR
170400000901      *
170500000901     c                   IF        *IN23 = *OFF
170600000901     C     ktah          chain     TITAH31C                           33
170700000901     C  N33ktahxs        chain     TITAS30C                           24
170800000901     c                   ENDIF
170900000901      *
171000000901     c                   DOW       *in33=*Off
171100000901     c                   DOW       *in24=*Off  and  Wendpag <> 'S'
171200000901     C                   exsr      CTR_selez
171300040217     c     Wendpag       cabeq     'S'           EndSel14
171400000901     C     ktahxs        reade     TITAS30C                               24
171500000901     C                   ENDDO
171600040217     c     Wendpag       cabeq     'S'           EndSel14
171700000901     C     ktah          reade     TITAH31C                               33
171800000901     C  N33ktahxs        chain     TITAS30C                           24
171900000901     C                   ENDDO
172000000901      *
172100040217     c     EndSel14      ENDSR
172200010219      *****************************************************************
172300010219      * CARICAMENTO SUBFILE * SELEZIONE 15 - numero ORM
172400010219      *****************************************************************
172500010219     C     RIESEL15      BEGSR
172600010219
172700010219      * Ricerco ORM in TITA4
172800010219
172900010219     c                   IF        *IN23 = *OFF
173000010219     C     ds_orm        chain     TITA432C                           33
173100010219     C  N33Ktas          chain     TITAS30C                           24
173200010219     c                   ENDIF
173300010219      *
173400010219     c                   DOW       *IN33 = *off
173500010219     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
173600010219     C                   exsr      CTR_selez
173700040217     c     Wendpag       cabeq     'S'           EndSel15
173800010219     C     Ktas          reade     TITAS30C                               24
173900010219     C                   ENDDO
174000040217     c     Wendpag       cabeq     'S'           EndSel15
174100010219     C     ds_orm        reade     TITA432C                               33
174200010219     C  N33Ktas          chain     TITAS30C                           24
174300010219     C                   ENDDO
174400010219
174500040217     C     EndSel15      ENDSR
174600000223      *****************************************************************
174700000223      * CONTROLLO SE SPEDIZIONE DPD NON ANCORA SELZIONATA
174800000223      *****************************************************************
174900000223     C     CTRspeddpd    BEGSR
175000000223      *
175100000223     c     TASlnp        lookup    FILDPD                                 30
175200000223     c  N30TASlna        lookup    FILDPD                                 30
175300000223      *
175400000223     c                   IF        *IN30 = *OFF
175500000223     c                   eval        *in31 = *on
175600000223     C                   ELSE
175700000223      *
175800000223     c                   eval      DSaas = TASaas
175900000223     c                   eval      DSlnp = TASlnp
176000000223     c                   eval      DSnrs = TASnrs
176100000223     c                   eval      DSnsp = TASnsp
176200000223      *
176300000223     c     DSsped        lookup    SPED                                   31
176400000223     c                   IF        *IN31 = *off
176500000223     c                   eval      xx = 1
176600000223     c     *zeros        lookup    SPED(xx)                               30
176700000223     c                   eval      SPED(xx) = DSsped
176800000223     c                   ENDIF
176900000223      *
177000000223     c                   ENDIF
177100000223      *
177200000223     c                   ENDSR
177300040216      *****************************************************************
177400040216      * CARICAMENTO SUBFILE * SELEZIONE 16 - Rif. alfabetico
177500040216      *****************************************************************
177600040216     C     RIESEL16      BEGSR
177700040216      *
177800040216     c                   IF        *IN23 = *OFF
177900040216     C     V1Crma        chain     TITA435C                           33
178000040216     C  N33Ktas          chain     TITAS30C                           24
178100040216     c                   ENDIF
178200040216      *
178300040216     c                   DOW       *IN33 = *off
178400040216     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
178500040216     C                   exsr      CTR_selez
178600040217     c     Wendpag       cabeq     'S'           EndSel16
178700040216     C     Ktas          reade     TITAS30C                               24
178800040216     C                   ENDDO
178900040217     c     Wendpag       cabeq     'S'           EndSel16
179000040216     C     V1Crma        reade     TITA435C                               33
179100040216     C  N33Ktas          chain     TITAS30C                           24
179200040216     C                   ENDDO
179300040216      *
179400040217     c     EndSel16      ENDSR
179500990618      *****************************************************************
179600990618      * CONTROLLO SE C.A. SELEZIONABILE PER CARICAMENTO SUBFILE
179700990618      *****************************************************************
179800990618     C     CTR_selez     BEGSR
179900990618
180000010111     C                   eval      *in32 = *off
180100990623      * Compongo la data spedizione per confronti
180200990623     C                   eval      wdatasp = (TASaas * 10000) + TASmgs
180300990623
180400990622      * determino lunghezza stringa ricerca destinatario
180500990622     C                   IF        V1Crsd <> *blanks
180600990622     c     ' '           checkr    V1Crsd        xx
180700990622     c                   ENDIF
180800990707
180900990707      * imposto segnacollo finale se zero
181000010112     C                   IF        TASnca = *zeros
181100010112     c                   z-add     TASncd        TASnca
181200010112     c                   ENDIF
181300000717      * imposto serie
181400000717     C                   IF        V1Cser <> *blanks
181500000717     c                   move      V1Cser        WorkSerie         2 0
181600000717     c                   ENDIF
181700000717
181800010129      * Bolle SI/NO/SOLO Poste
181900010129      * Non controllo se P.O. partenza o P.O. segnacollo impostati
182000010129      * e se accetto tutte le bolle (flag = "S")
182100010129     C                   eval      *in37 = *off
182200010129      *
182300010129     c                   IF        V1Cfls = *zeros  and  V1Clp1 = *zeros
182400030529     c                                              and  V1Csnp <> 'S'
182500010129     C     TASlnp        lookup    FILPOSTE                               30
182600000717      *
182700010129     c                   IF        *IN30 = *on  and  V1Csnp = 'N'
182800010129     C                   eval        *in37 = *on
182900010129     C                   ENDIF
183000010129      *
183100010129     c                   IF        *IN30 = *off  and  V1Csnp = 'P'
183200010129     C                   eval        *in37 = *on
183300010129     C                   ENDIF
183400000717      *
183500010129     C                   ENDIF
183600030529     C*
183700030529     C* Inserisco qui i controlli x verificare la data limite x considerare le
183800030529     C* bolle PT (tale controllo nn viene effettuato se l'utente è del gruppo
183900030529     C* EDP o ISP).
184000030529     C                   if        %subst(KPJBU:2:2) <> '05'
184100030529     C                   if        %subst(KNMUS:1:3) = 'EDP'
184200030529     C                   else
184300030529     C     tasLNP        lookup    FILPOSTE                               96
184400030529     C                   if        *in96
184500030529     C                   if        tasDCM >  0
184600030529     C                   if        tasDCM >= wDataLimitePT
184700030529     C                   else
184800030529     C                   seton                                        37
184900030529     C                   endif
185000030529     C                   endif
185100030529     C                   endif
185200030529     C                   endif
185300030529     C                   endif
185400030529     C*
185500990622
185600990618     c                   SELECT
185700990618      *
185800010129     C                   WHEN      *in37 = *on                                  POSTE SI/NO
185900000717      *
186000000717     C                   WHEN      V1Cksc <> *zeros  AND  V1Cksc <> TASksc      CODICE FATTURAZIONE
186100990618      *
186200990622     C                   WHEN      WV1Cdsd > *zeros  AND                        DATA SPEDIZIONE
186300990623     c                             (Wdatasp < WV1Cdsd or Wdatasp > WV1Cdsa)
186400990618      *
186500990622     C                   WHEN      V1Crsd <> *blanks  AND                       DESTINATARIO
186600990622     c                             %SUBST(V1CRSD:1:XX) <> %SUBST(TASRSD:1:XX)
186700990618      *
186800990622     C                   WHEN      V1Clp1 > *zeros  AND  V1Clp1 <> TASlnp       P.O. PARTENZA
186900000717      *
187000000717     C                   WHEN      V1Cser <> *blanks AND WorkSerie <> TASnrs    SERIE
187100990622      *
187200990622     C                   WHEN      V1Clna > *zeros  AND  V1Clna <> TASlna       P.O. ARRIVO
187300040216      *
187400040216     C                   WHEN      V1Ccbo > *blanks AND  V1Ccbo <> TAScbo       CODICE BOLLA
187500990623      *
187600990623      * I controlli per segnacollo devono restare alla fine
187700010112     C                   WHEN      V1Cnsc > *zeros  AND                          SELEZ. SEGNACOLLO
187800010112     C                               (V1Cfls <> TASfls  OR  V1Cnrr <> TASnrs)   P.O. E SERIE SEGNAC.
187900990618      *
188000010115     C                   WHEN      V1Cnsc > *zeros  AND  tasfns <> 'S'  AND        NUMERO SEGNACOLLO
188100010112     C                             TASncd > *zeros  AND  V1Cnsc < TASncd                 DAL
188200990623      *
188300010115     C                   WHEN      V1Cnsc > *zeros  AND  tasfns <> 'S'  AND        NUMERO SEGNACOLLO
188400010112     C                             TASnca > *zeros  AND  V1Cnsc > TASnca                AL
188500010115      *
188600010115     C                   WHEN      V1Cnsc > *zeros  AND  tasfns <> 'S'  and        NUMERO SEGNACOLLO
188700010115     C                             (V1Cnsc > TASnca  OR  V1Cnsc < TASncd)               AL
188800990618      *
188900990618     c                   OTHER
189000990623      *
189100010111      * indicatore di comodo per caricare la schiera nella riesel13
189200010115     C                   eval      *in32 = *on
189300010115     C                   exsr      RICsbfl
189400990618      *
189500990618     C                   ENDSL
189600990618      *
189700990618     c                   ENDSR
189800990623      **********************************************************************
189900990623      * SCRITTURA RCD SBFL
190000990623      **********************************************************************
190100990623     C     RICsbfl       BEGSR
190200990623      *
190300990618      * Carico
190400990622     c                   movel     *blanks       V2Copz
190500990622     c                   z-add     TASaas        V2Haas
190600990623     c                   z-add     TASaas        V2Cdsp
190700990623     c                   movel     TASmgs        V2Cdsp
190800990623     c     *mdy          move      V2Cdsp        DATA_eur
190900990623     c     *dmy          move      DATA_eur      V2Cdsp
191000990622     c                   z-add     TASlnp        V2Clnp
191100990622     c                   z-add     TASnrs        V2Cnrs
191200990622     c                   z-add     TASnsp        V2Cnsp
191300990622     c                   movel     TAStbl        V2Ctbl
191400990622     c                   z-add     TASlna        V2Clna
191500990622     c                   z-add     TASnft        V2Cnft
191600990622     c                   IF        TASdft = *zeros
191700990707     c                   clear                   V2Cdft
191800990622     c                   ELSE
191900990624     c     *iso          move      TASdft        DATA_eur
192000990707     c     *dmy          move      DATA_eur      W0060
192100990707     C                   eval      V2Cdft = %EDITC(W0060:'Y')
192200990622     c                   ENDIF
192300990622     c                   z-add     TASrmn        V2Crmn
192400990622     c                   z-add     TASksc        V2Cksc
192500990622     c                   z-add     TASctr        V2Cctr
192600990622     c                   movel     TAStsp        V2Ctsp
192700990622     c                   movel     TASrsd        V2Crsd
192800990707     c                   IF        TASdcm = *zeros
192900990707     c                   clear                   V2Cdcm
193000990707     c                   ELSE
193100990707     c     *iso          move      TASdcm        DATA_eur
193200990707     c     *eur          move      DATA_eur      W0080
193300990707     C                   eval      V2Cdcm = %EDITC(W0080:'Y')
193400990707     c                   ENDIF
193500990618      *
193600990622      * Ricavo ragione sociale mittente
193700990622     c                   clear                   V2Crsc
193800990708     c                   move      TASccm        W0040
193900990623     c                   IF        W0040 = 9999  or  W0040 = 8888
194000990623     c                                           or  W0040 = *zeros
194100990629     c                   eval      ktrc = 'M'
194200990707     c     KTAA          CHAIN     TITAA30C
194300990707     c                   IF        %FOUND
194400990622     c                   movel     TAArsc        V2Crsc
194500990618     c                   ENDIF
194600990622      *
194700990622     c                   ELSE
194800990622     c                   z-add     TASccm        I69kac
194900990622     c                   exsr      CTR_anagra
195000990707     C                   If         WO69ERR <> '1'
195100990622     c                   movel     ACOrag        V2Crsc
195200990622     c                   Endif
195300990622     c                   ENDIF
195400990618      *
195500990623     c                   add       1             NUMspedcar
195600990618     c                   add       1             NRR1
195700990623     c                   add       1             Wspedpag
195800990618      *
195900990622      * posizione cursore pagina nuova per ROLLUP carico 18 rcd per volta
196000990618     c                   SELECT
196100990623     c                   WHEN      Wspedpag = 1
196200990618     c                   eval      nrr23 = nrr1
196300990623     c                   WHEN      Wspedpag = 18
196400990618     c                   eval      Wendpag = 'S'
196500990618     c                   ENDSL
196600990618      *
196700990623     c                   write     SB50S02
196800990618      *
196900990622     c                   ENDSR
197000990618      **********************************************************************
197100990618      * EMISSIONE VIDEO 2
197200990618      **********************************************************************
197300990618     C     EMISD02       BEGSR
197400990618      *
197500990618     c                   SELECT
197600990618     c                   WHEN      *IN23 = *ON  and NRR23 > *zeros
197700990618     c                   z-add     nrr23         V2Cnrr
197800990618     c                   WHEN      *IN90 = *ON  and NRR1 > *zeros
197900990618     c                   z-add     nrr1          V2Cnrr
198000990618     c                   OTHER
198100990618     c                   z-add     1             V2Cnrr
198200990618     c                   ENDSL
198300990618      *
198400990730     c                   eval      *in20 = *on
198500990730     c                   eval      *in21 = *on
198600990618      *
198700990623     c                   write     SB50T01
198800990623     c                   write     SB50Z02
198900990623     c                   exfmt     SB50C02
199000990618      *
199100990730     c                   eval      *in90 = *off
199200990730     c                   eval      *in28 = *off
199300990623     c                   clear                   v1cmsg
199400990618      *
199500990618     C                   ENDSR
199600990618      **********************************************************************
199700990618      * CONTROLLI VIDEO 2
199800990618      **********************************************************************
199900990618     C     Ctrd02        BEGSR
200000990618      *
200100990624     c                   clear                   wselsped
200200990802     c                   clear                   nrr60
200300990618     c                   z-add     1             nrr2
200400990618      *
200500990706     c     NRR2          CHAIN     SB50S02
200600990618      *
200700990706     c                   DOW       %FOUND  and  *IN90 = *OFF
200800990618
200900990618     c                   SELECT
201000990618
201100990630     c                   WHEN      V2Copz = *blanks
201200990630
201300990630     c                   WHEN      V2Copz = '5'
201400990623     c                   exsr      SUB_opz05
201500990623     c                   clear                   V2Copz
201600990706     c                   eval      *in90 = (O51F03 = '1')                       per F3 torno al SFL
201700990802     c                   eval      *in60 = *on
201800990802     c                   z-add     NRR1          NRR60
201900041012      * se selezionata bolla vado a fine
202000110520     c                   if        not *in90
202100110520     c                   eval      *IN90 = (wselsped = '1' and o51f03 <> *on)
202200110520     c                   endif
202300990802
202400990705     c                   WHEN      V2Copz = '1'  and  V2Copz = V2Dsel           Selezione
202500990624     c                   z-add     V2Haas        D50aas
202600990624     c                   z-add     V2Clnp        D50lnp
202700990624     c                   z-add     V2Cnrs        D50nrs
202800990624     c                   z-add     V2Cnsp        D50nsp
202900990624     c                   movel     V2Ctbl        D50tbl
203000041008      * se richiamato dai reclami
203100041008     c                   If        sdsprm > 1
203200041008     c                   eval      IA1ins = 'S'
203300041008     c                   eval      IA1tor = 'S'
203400041008     c                   eval      IA1ogg = ds_OggSped
203500041008e   3c                   endif
203600041008      *
203700990624     c                   eval      *IN90 = *ON
203800990624     c                   eval      wselsped = '1'
203900990705
204000990705     c                   WHEN      V2Copz = '2'  and  V2Copz = V2Dsel           Gestione
204100990726     c                   exsr      SUB_opz02
204200990726     c                   clear                   V2Copz
204300990726     c                   eval      *in90 = (D01F03 = '1')                       per F3 torno al SFL
204400990802     c                   eval      *in60 = *on
204500990802     c                   z-add     NRR1          NRR60
204600990726     c                   if        D01msg <> *blanks
204700990726     c                   movel     D01msg        V1Cmsg
204800990730     c                   eval      *in90 = *on
204900990730     c                   eval      *in28 = *on
205000990726     c                   endif
205100990618
205200990623     c                   OTHER                                                  Opzione errata
205300990623     c                   movel     MSG(20)       V1Cmsg
205400990730     c                   eval      *in28 = *on
205500990802     c                   eval      *in90 = *on
205600990730     c                   eval      *in60 = *on
205700990802     c                   z-add     NRR1          NRR60
205800990618
205900990618     c                   ENDSL
206000990618      *
206100990623     c                   update    SB50S02
206200990802     c                   eval      *IN60 = *OFF
206300990618      *
206400990618     c                   IF        *IN90 = *OFF
206500990618     c                   add       1             nrr2
206600990706     c     NRR2          CHAIN     SB50S02
206700990618     c                   ENDIF
206800990618      *
206900990618     c                   enddo
207000990618      *
207100990802      * Se devo posizionarmi senza errore accendo IN90
207200990802     c                   IF        NRR60 <> *zeros
207300990802     c                   z-add     NRR60         NRR1
207400990802     c                   eval      *IN90 = *on
207500990802     c                   ENDIF
207600990802      *
207700990618     C                   ENDSR
207800990618      **********************************************************************
207900990623      * PREPARAZIONE DS PASSAGGIO PARAMETRI PER SELEZIONE 5
208000990618      **********************************************************************
208100990623     C     SUB_OPZ05     BEGSR
208200990618      *
208300990624     c                   clear                   TNSB51DS
208400990624      *
208500990705     c                   movel     I50OP0        I51OP0
208600990705     c                   movel     I50OP1        I51OP1
208700990705     c                   z-add     V2Haas        I51aas
208800990624     c                   z-add     V2Clnp        I51lnp
208900990624     c                   z-add     V2Cnrs        I51nrs
209000990624     c                   z-add     V2Cnsp        I51nsp
209100990624     c                   movel     V2Ctbl        I51tbl
209200990618      *
209300990624     C                   movel     TNSB51DS      Kpjbu
209400041008      * se richiamato dai recalmi passo anche
209500041008     c                   if         SDSprm > 1
209600041008      *
209700990624     c                   call      'TNSB51R'
209800990624     c                   parm                    KPJBA
209900041008     c                   parm                    FIDNA1ds
210000041012      *
210100041012      * se richiamato da reclami e selezionato la bolla vado a fine
210200041012if  1c                   if            SDSprm > 1
210300041012     c                             and IA1ins = 'S'
210400041012     c                   eval      Wselsped = '1'
210500041012e   1c                   endif
210600041008      *
210700041008     c                   else
210800041008      *
210900041008     c                   call      'TNSB51R'
211000041008     c                   parm                    KPJBA
211100041008      *
211200041008     c                   endif
211300990624      *
211400990624     C                   movel     KPJBU         TNSB51DS
211500990624      *
211600990618     C                   ENDSR
211700990726      **********************************************************************
211800990726      * PREPARAZIONE DS PASSAGGIO PARAMETRI PER SELEZIONE 2
211900990726      **********************************************************************
212000990726     C     SUB_OPZ02     BEGSR
212100990726      *
212200990726     c                   clear                   TNSB01DS
212300990726      *
212400990726     c                   movel     ' 02'         D01OP0
212500990726     c                   z-add     V2Haas        D01aas
212600990726     c                   z-add     V2Clnp        D01lnp
212700990726     c                   z-add     V2Cnrs        D01nrs
212800990726     c                   z-add     V2Cnsp        D01nsp
212900990726     c                   movel     V2Ctbl        D01tbl
213000990726      *
213100990726     C                   movel(P)  TNSB01DS      Kpjbu
213200990726      *
213300990726     c                   call      'TNSB52R'
213400990726     c                   parm                    KPJBA
213500990726      *
213600990726     C                   movel     KPJBU         TNSB01DS
213700990726      *
213800990726     C                   ENDSR
213900990621      *****************************************************************
214000990621      *  AGGANCIO ANAGRAFICHE
214100990621      *****************************************************************
214200990621     C     CTR_ANAGRA    BEGSR
214300990621      *
214400990707     c                   clear                   WO69ERR
214500990707      *
214600990621     C                   CALL      'TIBS69R'
214700990621     C                   PARM                    tibs69DS
214800990621     C                   PARM                    DS_cnaco
214900990621     C                   PARM                    DS_cnind
215000990621     C                   PARM                    DS_cnclp
215100990621     C                   PARM                    DS_fncls
215200990621      *
215300990621     C                   eval      wtibs69r = 'S'
215400990707     C                   eval      WO69ERR =  O69ERR
215500990621      * per il 1° giro è inizializzata nelle specifiche "D"
215600990621     C                   clear                   TIBS69DS
215700990621      *
215800990621     C                   ENDSR
215900990618      *****************************************************************
216000990618      *  Ricerca alfabetica
216100990618      *****************************************************************
216200990618     c     ricalf        begsr
216300990618      *
216400990618     C                   MOVEL     rsut          PAXdut                         Descrizione Azienda
216500990618     C                   Z-ADD     9             PAXsta                         9=Escludo Annullati
216600030529     C                   Z-ADD     DUTKCI        PAXccc                         Capoconto Clienti
216700000901     C                   z-add     1             PAXNUM
216800990618      *
216900000901     C                   CALL      'XALFA3BR'
217000990618     C                   PARM                    PAXDUT
217100990618     C                   PARM                    CODUT
217200990618     C                   PARM                    PAXDMT
217300990618     C                   PARM                    PAXCCC
217400990618     C                   PARM                    PAXSTA
217500990618     C                   PARM      *blanks       PAXFLR
217600990618     C                   PARM      *blanks       PAXDIT
217700000901     C                   PARM                    PAXNUM            2 0
217800000901     C                   PARM                    PAXKCM           80
217900000901     C                   PARM                    PAXKSM          140
218000000901     C                   PARM                    PAXKDM           60
218100990618      *
218200990618     c                   endsr
218300990805      **********************************************************************
218400990805      * CONTROLLI STANDARD    * data immessa a video *
218500990805      **********************************************************************
218600990805     C     CTR_data      BEGSR
218700990805      *
218800990805     c     *eur          TEST(DE)                WDATA_08
218900990805     c                   IF        %error = *off
219000990805     c     *eur          movel     WDATA_08      DATA_EUR
219100990805     c     *iso          movel     DATA_eur      WDATA_08G
219200990805     c                   ELSE
219300990805     c     *dmy          TEST(DE)                WDATA_08
219400990805     c                   IF        %error = *off  and  WDATA_02 = *zeros
219500990805     c     *dmy          movel     WDATA_06      DATA_EUR
219600990805     c     *eur          movel     DATA_eur      WDATA_08
219700990805     c     *iso          movel     DATA_eur      WDATA_08G
219800990805     c                   ELSE
219900990805     c                   eval      *IN28 = *on
220000990805     c                   ENDIF
220100990805     c                   ENDIF
220200990805      *
220300990805     c                   ENDSR
220400990618      **********************************************************************
220500990618      * CONTROLLI STANDARD    * linea *
220600990618      **********************************************************************
220700990618     C     CTR_linea     BEGSR
220800990618      *
220900990623     C                   IF        Wlinea = *zeros
221000990618     c                   MOVEL     MSG(6)        v1CMSG
221100990618     c                   eval      *in28 = *on
221200990618     c                   eval      *in90 = *on
221300990618     c                   ELSE
221400990618      *
221500990707     c     Wlinea        chain     azorg01l
221600990707     c                   if        not %FOUND  or  ORGfva <> *blanks
221700990618     c                   MOVEL     MSG(7)        v1CMSG
221800990618     c                   eval      *in28 = *on
221900990618     c                   eval      *in90 = *on
222000990618     c                   endif
222100990618     c                   ENDIF
222200990618      *
222300990618     c                   ENDSR
222400990618      **********************************************************************
222500990618      * CONTROLLI STANDARD    * anno *
222600990618      **********************************************************************
222700990618     C     CTR_anno      BEGSR
222800990618      *
222900990623     c                   IF        Wanno = *zeros
223000990618     c                   MOVEL     MSG(9)        v1CMSG
223100990730     c                   eval      *in90 = *on
223200990730     c                   eval      *in28 = *on
223300990618     C                   else
223400990618      *
223500990618      *   sistemo anno di due cifre
223600990623     C                   if        Wanno < 100  and Wanno > 60
223700990623     C                   ADD       1900          Wanno
223800990618     C                   endif
223900990623     C                   if        Wanno < 100  and Wanno <= 60
224000990623     C                   ADD       2000          Wanno
224100990618     C                   endif
224200990618      *
224300990618     C                   ENDIF
224400990618      *
224500990618     c                   ENDSR
224600030529     C*--------------------------------------------------------------------------------------------*
224700030529     C* CARTAB - CARICA I DATI TABELLATI
224800030529     C*--------------------------------------------------------------------------------------------*
224900030529     C     CARTAB        BEGSR
225000030529     C*
225100030529     C* Partendo dalla data attuale reperisco la data limite olre la quale
225200030529     C* nn scendere nel considerare le bolle PT.
225300030529     C                   eval      wrkoggiiso = *loval
225400030529     C*
225500030529     C                   eval      tbeCOD = 'VPO'
225600030529     C                   eval      tbeKE1 = 'VPO'
225700030529     C     KTBE01P       chain     TNTBE01L
225800030529     C                   if        %found(TNTBE01L)
225900030529     C                   eval      DVPO = tbeUNI
226000030529     C*
226100030529     C* A seconda che l'utente sia di SEDE o d Filiale considero due valori diversi x i giorni
226200030529     C* dalla data attuale entro i quali considerare le bolle PT >>> CONSEGNATE <<<
226300031114      * l'utente ASC99 è inserito come utente di sede, ma di fatto è un utente di filiale
226400031114     C                   if        DUTPOU <> 046 or Knmus = 'ASC99     '
226500030529     C                   eval      wGGLimPT = §VPOIPP
226600030529     C                   else
226700030529     C                   eval      wGGLimPT = §VPOIPS
226800030529     C                   endif
226900030529     C*
227000030529     C* Se il valore in tabella è 9999 significa nessun blocco data limite
227100030529     C                   if        wGGLimPT <> 9999
227200030529     C                   time                    wrkoggiiso
227300030529     C     wrkoggiiso    subdur    wGGLimPT:*DAYSwrkoggiiso
227400030529     C                   endif
227500030529     C                   endif
227600030529     C     *iso          move      wrkoggiiso    wDataLimitePT
227700030529     C*
227800030529     C                   ENDSR
227900030529     C*--------------------------------------------------------------------------------------------*
228000030529     C* REPDATIUTE - REPERISCE DATI UTENTE
228100030529     C*--------------------------------------------------------------------------------------------*
228200030529     C     REPDATIUTE    BEGSR
228300030529     C*
228400030529     C* Reimposta le variabili di lavoro
228500030529     C                   CLEAR                   TIBS34DS
228600030529     C                   CLEAR                   AZUTEDS
228700030529     C                   CLEAR                   DDATIUTE
228800030529     C*
228900030529     C* Reperisce i dati del profilo in ingresso
229000030529     C     *DTAARA       DEFINE    §azute        azuteds
229100030529     C     *DTAARA       DEFINE    §datiute      ddatiute
229200030529     C                   IN(E)     *DTAARA
229300030529     C                   IF        %Error                                       *NO errori
229400030529     C                   EVAL      I34Tla = 'L'
229500030529     C                   CALL      'TIBS34R'
229600030529     C                   PARM                    TIBS34DS
229700030529     C                   IN        *DTAARA
229800030529     C                   ENDIF
229900030529     C*
230000030529     C                   ENDSR
230100990618      *****************************************************************
230200990618      * ROUTINE INIZIALE
230300990618      *****************************************************************
230400990618     C     *INZSR        BEGSR
230500990618      *
230600990618     C     *ENTRY        PLIST
230700990618     C                   PARM                    KPJBA
230800041008     C                   PARM                    fidna1ds
230900030529     C*
231000030529     C* REPERISCE I DATI UTENTE
231100030529     C                   EXSR      REPDATIUTE
231200030529     C*
231300030529     C* CARICA I DATI TABELLATI
231400030529     C                   EXSR      CARTAB
231500000718      * ! metodo
231600000718     C                   z-add     2             metodo            1 0
231700000718      *
231800990624      *--- DATI UTENTE/AZIENDA
231900030529     C                   Z-ADD     1             CODUT             1 0
232000000223
232100010129      * Carico schiera con P.O. DPD e P.O. POSTE
232200000223     c                   clear                   xx
232300000223     C     *loval        setll     azorg
232400000223     C                   read      azorg
232500000223      *
232600000223     C                   DOW       NOT %EOF
232700000223      *
232800010129     c                   movel     ORGde3        OG143
232900010129      *
233000010129     c                   SELECT
233100010129     c                   WHEN      ORGFVA <> *blanks
233200020211     C                   WHEN      §ogntw = 'DPD'
233300000223     C                   add       1             XX
233400000223     c                   z-add     ORGFIL        FILDPD(XX)
233500020211     C                   WHEN      §ogntw= 'PPT'
233600010129     C                   add       1             YY
233700010129     c                   z-add     ORGFIL        FILPOSTE(YY)
233800010129     C                   ENDSL
233900000223      *
234000000223     C                   read      azorg
234100000223     C                   ENDDO
234200990618
234300990618      * reperisco data e ora
234400990618     C                   TIME                    W0140
234500990618      * UDATE IN GGMMAAAA
234600990618     C                   MOVE      W0140         WDTGIO
234700990618      * UDATE IN AAAAMMGG
234800990805     C     *eur          MOVEL     WDTGIO        DATA_eur
234900990805     C     *iso          MOVEL     DATA_eur      dateu
235000990618
235100990618     C     kspe          KLIST
235200990622     C                   KFLD                    v1cAAS
235300990622     C                   KFLD                    v1cLNP
235400990622     C                   KFLD                    v1cNRS
235500990622     C                   KFLD                    v1cNSP
235600990618      *
235700990623     C     kspe1         KLIST
235800990623     C                   KFLD                    v1cAAS
235900990623     C                   KFLD                    v1cLNP
236000990623     C                   KFLD                    v1cNRS
236100990623     C                   KFLD                    v1cNSP
236200990623     C                   KFLD                    D50tbl
236300990813
236400990813     C     kspe9         KLIST
236500990813     C                   KFLD                    v1cAAS
236600990813     C                   KFLD                    v1cLP1
236700990623      *
236800990622     C     krmn          KLIST
236900990622     C                   KFLD                    v1cRMN
237000990622     C                   KFLD                    v1cCCM
237100990622      *
237200990707     C     kfat1         KLIST
237300990707     C                   KFLD                    v1cKSC
237400990707      *
237500990707     C     kfat2         KLIST
237600990622     C                   KFLD                    v1cKSC
237700990716     C                   KFLD                    Wv1cDFT
237800990622      *
237900990707     C     kfat3         KLIST
238000990622     C                   KFLD                    v1cKSC
238100990716     C                   KFLD                    Wv1cDFT
238200990622     C                   KFLD                    v1cNFT
238300060608     C     kfat4         KLIST
238400060608     C                   KFLD                    v1cfiv
238500060608     C                   KFLD                    v1cNFT
238600060608     C     kfat43        KLIST
238700060608     C                   KFLD                    v1cfiv
238800060608     C                   KFLD                    v1cNFT
238900060608     C                   KFLD                    Wv1cDFT
239000990622      *
239100990622     C     kdta          KLIST
239200010112     C                   KFLD                    klinea
239300000717     C                   KFLD                    KAAS
239400990622     C                   KFLD                    KMGS
239500000718      *
239600000718     C     kdta1         KLIST
239700010112     C                   KFLD                    klinea
239800000718     C                   KFLD                    KAAS
239900000718     C                   KFLD                    KMGS
240000000718     C                   KFLD                    WorkSerie
240100000718      *
240200000718     C     kdta2         KLIST
240300010112     C                   KFLD                    klinea
240400000718     C                   KFLD                    KAAS
240500000718     C                   KFLD                    KMGS
240600000718     C                   KFLD                    WorkSerie
240700000718     C                   KFLD                    KSEGNA
240800000223      *
240900000223     C     ktas          KLIST
241000000223     C                   KFLD                    ta4AAS
241100000223     C                   KFLD                    ta4LNP
241200000223     C                   KFLD                    ta4NRS
241300000223     C                   KFLD                    ta4NSP
241400990622      *
241500990629     C     ktaa          KLIST
241600990622     C                   KFLD                    tasAAS
241700990622     C                   KFLD                    tasLNP
241800990622     C                   KFLD                    tasNRS
241900990622     C                   KFLD                    tasNSP
242000990629     C                   KFLD                    Ktrc
242100990623      *
242200990623     C     ktat          KLIST
242300010112     C                   KFLD                    v1cfls
242400010112     C                   KFLD                    v1cnrr
242500010112     C                   KFLD                    v1cnsc
242600010112     C                   KFLD                    v1clna
242700000717      *
242800000717     C     ktat1         KLIST
242900010112     C                   KFLD                    v1cfls
243000010112     C                   KFLD                    v1cnrr
243100010112     C                   KFLD                    v1cnsc
243200000717      *
243300010112     C     ktats         KLIST
243400000717     C                   KFLD                    TATAAS
243500000717     C                   KFLD                    TATLNP
243600000717     C                   KFLD                    TATNRS
243700000717     C                   KFLD                    TATNSP
243800000717      *
243900000717     C     kspe11        KLIST
244000000717     C                   KFLD                    v1cLP1
244100000717     C                   KFLD                    v1cRSD
244200000717      *
244300000717     C     kspe12        KLIST
244400000717     C                   KFLD                    kAAS
244500000717     C                   KFLD                    v1cLP1
244600000717     C                   KFLD                    WorkSerie
244700000901      *
244800000901     C     ktah          KLIST
244900000901     C                   KFLD                    KKKTRC            1
245000000901     C                   KFLD                    V1Cnot
245100000901      *
245200000911     C                   MOVE      'C'           KKKTRC                         "Chi Sono"
245300000901      *
245400000901     C     ktahxs        KLIST
245500000901     C                   KFLD                    TAHAAS
245600000901     C                   KFLD                    TAHLNP
245700000901     C                   KFLD                    TAHNRS
245800000901     C                   KFLD                    TAHNSP
245900030529     C*
246000030529     C* Chiave su TNTBE01L - Parziale
246100030529     C     KTBE01P       KLIST
246200030529     C                   KFLD                    TBECOD
246300030529     C                   KFLD                    TBEKE1
246400040216      *
246500040216      * Chiave su TABEL00F
246600040216     c     K03TAB00      klist
246700040216     c                   kfld                    TBLkut
246800040216     c                   kfld                    TBLcod
246900040216     c                   kfld                    TBLkey
247000990623
247100990618     C                   ENDSR
247200990618      *---------------------------------------------------------------------------------------------
247300990618** MSG  Lungh. 78                                                            *
247400990802TNSB50R- Livello procedura chiamante non valida. Avvertire il C.E.D.            1
247500990623TNSB50R- Codice cliente errato                                                  2
247600990623TNSB50R- Non richiedere il cod. mittente se non si è immesso il rif. mittente   3
247700990623TNSB50R- Impostate selezioni incongruenti                                       4
247800990623TNSB50R- Data fattura errata                                                    5
247900990623TNSB50R- Immettere la linea                                                     6
248000990623TNSB50R- Linea inesistente o annullata                                          7
248100060608Non richiedere data, numero fattura o P.O.IVA e le spedizioni NON fatturate           8
248200990623TNSB50R- Immettere l'anno spedizione                                            9
248300060608Se immesso numero FATTURA richiedere anche cod.Cliente Fatturazione o P.O. IVA
248400060608Se richiesta Data Fattura immettere Cod.fatturazione o P.O IVA e NumeroFattura
248500990623TNSB50R- Non richiedere sped. non fatturate se non è immesso il cod. fatturaz.  12
248600990623TNSB50R- Data spedizione errata                                                 13
248700990623TNSB50R- Immettere data spedizione DAL minore o uguale a data spedizione AL     14
248800990623TNSB50R- Immettere almeno una selezione valida                                  15
248900990623TNSB50R- Non richiedere la serie se non immessi anche P.O. e numero segnacollo  16
249000990623TNSB50R- Non richiedere il P.O. segnacollo se non immesso il numero segnacollo  17
249100990623TNSB50R- Non richiedere il numero segnacollo se non immesso il P.O. segnacollo  18
249200990623TNSB50R- Bolla esistente ma annullata                                           19
249300990623TNSB50R- Opzione non valida                                                     20
249400990623TNSB50R- Scorrimento indietro o in avanti oltre il primo o l'ultimo record      21
249500990624TNSB50R- Non trovata la spedizione richiesta                                    22
249600990726TNSB50R- Immettere il numero spedizione                                         23
249700000714TNSB50R- Immettere un valore numerico                                           24
249800000714TNSB50R- Immettere anche il P.O. Partenza                                       25
249900010129TNSB50R- Non richiedere la serie, altre serie inserite                          26
250000000717TNSB50R- Immettere anche la data di spedizione                                  27
250100010112TNSB50R- Non richiedere la linea se non immessi anche il numero segnacollo      28
250200000718TNSB50R- Immettere al massimo il mese successivo della Data Spedizione dal      29
250300010112TNSB50R- Immettere anche il P.O. Arrivo- (serie è > di 0)                       30
250400010219TNSB50R- Immettere il numero ORM                                                31
250500010219TNSB50R- Immettere il p.o. emissione                                            32
250600010219TNSB50R- P.O. emissione inesistente o annullato                                 33
250700010219TNSB50R- Non richiedere serie o viaggio se non inserito il n.orm                34
250800040216TNSB50R- Codice bolla errato                                                    35
250900060609Se immesso P.O. IVA immettere anche il Numero Fattura                           36
251000060609Se immesso P.O. IVA non immettere il cod.cliente fatturazione e viceversa       37
251100060609Se immesso cod.Cliente Fatturazione e numero fattura immettere anche la DATA    38
251200060614Parcel DPD formalmente errato: o lungo 11 numerico o lungo 14 alfanumerico      39
251300060609**
2514000606090123456789
