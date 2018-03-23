000100040721      *===============================================================*
000200040721      * TNSB10R    * Aggiornamento TITAS per immagine LdV             *
000300040721      *===============================================================*
000400040721
000500040721     h Decedit('0,') Datedit(*dmy/) Option(*nodebugio)
000600040721
000700040721      *===============================================================*
000800040721
000900041021     fTITAS26C  if   e           k disk
001000060605     fTITA430c  if   e           k disk
001100040721     fTILAO01L  uf a e           k disk
001200040721
001300040721      *===============================================================*
001400040721
001500040721      * Strutture Dati - - - - - - - - - - - - - - - - - - - - - - - -*
001600040721      *
001700040721     d KPJBA         e ds
001800060605     d dsbl4i        e ds
001900040721      * - Parametri x Controllo profilo utenti
002000040721     d TIBS34DS      e ds                  inz
002100040721      * - Ds di riferimento al file esterno AZUTE00F
002200040721     d AZUTEDS       e ds                  extname(AZUTE00F)
002300040721      * - Ds per dati organigramma
002400040721     d DDatiUte      e ds
002500040721      * - Ds per campo TASFLO del file TITAS30C
002600040721     d dTASflo       e ds                  inz
002700040721      *
002800040721     d Status         sds           333
002900040721     d   SDSpgm          *proc
003000040721     d   SDSjob              244    253                                         Job name
003100040721     d***SDSusr              254    263                                         User name
003200040721     d***SDSjnr              264    269S 0                                      Job number
003300040721      *
003400040721     d TNSB14ds        ds                  inz
003500040721     d   D14dcd                       8  0 inz
003600040721     d   D14dca                       8  0 inz
003700040721      *
003800040721     d WLBdat          ds                  inz
003900040721     d   G08dat                       8  0 inz
004000040721     d   G08inv                       8  0 inz
004100040721     d   G08err                       1    inz(*off)
004200040721     d   G08tgi                       5  0 inz
004300040721      *
004400040721     d DS_Time1        ds            14    inz
004500040721     d   dsT1_hms                     6  0 inz
004600040721     d   dsT1_dmy                     8  0 inz
004700040721     d DS_Time2        ds            14    inz
004800040721     d   dsT2_ymd                     8  0 inz
004900040721     d   dsT2_hms                     6  0 inz
005000040721      *
005100040721      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
005200040721      *
005300040721      * - Campi di comodo
005400040721     d WdateISO        s               d   datfmt(*ISO) inz
005500040721     d Wtime           s             14  0              inz
005600040721     d WtasDCM         s              8                 inz(*zeros)
005700060606     d WtasRMN         s             15                 inz(*zeros)
005800060605     d ktrc            s                   like(ta4trc) inz('I')
005900040721
006000040721      *===============================================================*
006100040721      *  RIEPILOGO INDICATORI                                         *
006200040721      *---------------------------------------------------------------*
006300040721      * 10    - Comodo                                                *
006400040721      *===============================================================*
006500040721
006600040721     c     *Entry        plist
006700040721     c                   parm                    KPJBA
006800040721     c                   movel     KPJBU         TNSB14ds
006900040721      *
007000040721      * Operazioni iniziali
007100040721     c                   exsr      OperazIniz
007200040721      *
007300041021      * Ciclo di lettura del file TITAS26C
007400041021     c     D14dcd        setll     TITAS26C
007500041021     c                   read      TITAS26C
007600040721      *
007700041021do  1c                   dow       NOT %eof(TITAS26C)
007800040721     c                             and TASdcm <= D14dca
007900040721      *
008000040721     c                   movel     TASflo        dTASflo
008100040721      * - Elaborazione delle sole LdV con immagine inviata a DPD
008200040721if  2c                   if        §FLOiml  = 'I'
008300040721     c                   exsr      Wrt_TILAO
008400040721e   2c                   endif
008500040721      *
008600041021      * - Lettura del record successivo nel file TITAS26C
008700041021     c                   read      TITAS26C
008800040721      *
008900040721e   1c                   enddo
009000040721      *
009100040721      * Fine
009200040721     c                   eval      *inLR = *on
009300040721      *
009400040721      *---------------------------------------------------------------*
009500040721      * Operazioni iniziali                                           *
009600040721      *---------------------------------------------------------------*
009700040721     c     OperazIniz    BEGSR
009800040721      *
009900040721      * Definizione Key-List
010000040721      * - TILAO01L
010100040721     c     K05LAO01      klist
010200040721     c                   kfld                    LAOaas
010300040721     c                   kfld                    LAOlnp
010400040721     c                   kfld                    LAOnrs
010500040721     c                   kfld                    LAOnsp
010600040721     c                   kfld                    LAOcop
010700040721      *
010800060605      * - tita430c
010900060605     c     Kta4          klist
011000060605     c                   kfld                    tasaas
011100060605     c                   kfld                    taslnp
011200060605     c                   kfld                    tasnrs
011300060605     c                   kfld                    tasnsp
011400060605     c                   kfld                    ktrc
011500040721      * Reperisco dati job
011600040721     c                   exsr      DatiJob
011700040721      *
011800040721      * Impostazione del campo LAOTIM = aaaa/mm/gg+hh:mm:ss
011900040721     c                   time                    Wtime
012000040721     c                   movel     Wtime         DS_Time1
012100040721     c                   eval      dsT2_hms   = dsT1_hms
012200040721     c     *eur          movel     dsT1_dmy      WdateISO
012300040721     c                   move      WdateISO      dsT2_ymd
012400040721      *
012500040721     c                   ENDSR
012600040721      *
012700040721      *---------------------------------------------------------------*
012800040721      * Reperimento Dati del job (Utente/Operativi)                   *
012900040721      *---------------------------------------------------------------*
013000040721     c     DatiJob       BEGSR
013100040721      *
013200040721     c     *dtaara       define    §azute        azuteds
013300040721     c     *dtaara       define    §datiute      ddatiute
013400040721      *
013500040721     c                   in(E)     *dtaara
013600040721     c                   IF        %ERROR or RSUT = *blanks
013700040721     c                   clear                   Tibs34Ds
013800040721     c                   call      'TIBS34R'
013900040721     c                   parm                    Tibs34Ds
014000040721     c                   in        *dtaara
014100040721     c                   ENDIF
014200040721      *
014300040721     c                   ENDSR
014400040721      *
014500040721      *---------------------------------------------------------------*
014600040721      * Registrazione dati nel file TILAO00F x DPD                    *
014700040721      *---------------------------------------------------------------*
014800040721     c     Wrt_TILAO     BEGSR
014900040721      *
015000040721      * Impostazione dei campi numerici in campi alfanumerici di comodo
015100040721     c                   movel     TASdcm        WtasDCM
015200060606     c                   move      TASrmn        WtasRMN
015300040721      *
015400040721      * Aggancio record corrispondente nel file TILAO00F
015500040721     c                   clear                   TILAO000
015600040721     c                   move      TASaas        LAOaas
015700040721     c                   move      TASlnp        LAOlnp
015800040721     c                   move      TASnrs        LAOnrs
015900040721     c                   move      TASnsp        LAOnsp
016000040721     c                   movel     'DPD'         LAOcop
016100040721     c     K05LAO01      chain     TILAO000
016200040721      *
016300040721      * Impostazione nuovi dati
016400040721     c                   eval      LAOtim  = DS_Time2
016500040721     c                   eval      LAGidl  = SDSjob
016600040721     c                   eval      LAGela  = *zeros
016700040721      *
016800040721      * Aggiornamento record
016900040721if  1c                   if        NOT %found(TILAO01L)
017000060605     c                   clear                   dsbl4i
017100060605     c     kta4          chain     tita430c
017200060605     c                   if        %found(tita430c)
017300060605     c                   movel     ta4not        dsbl4i
017400060605     c                   endif
017500040721     c***                clear                   LAOxx1
017600060606     c                   clear                   w014a            14
017700060606     c* se non trovato numero parcel su tita4 t.rk "I" lo prendo da rmn
017800060606     c                   if        §b4ipn = *blanks
017900060606     c* numero parcel lungo 11 lo trucco con 0 davanti al depot e 99 davanti
018000060606     c* al progressivo
018100060606     c                   if        %subst(wtasrmn:2:1) = '0'
018200060606     c                   eval      w014a = '0'
018300060606     c                                   + %subst(wtasrmn:4:3)
018400060606     c                                   + '99'
018500060606     c                                   + %subst(wtasrmn:7:8)
018600060606     c                   else
018700060606     c                   movel     wtasrmn       w014a
018800060606     c                   endif
018900060606     c                   else
019000060606     c                   if        %subst(§b4ipn:12:1) = ' '
019100060606     c                   eval      w014a = '0'
019200060606     c                                   + %subst(§b4ipn:1:3)
019300060606     c                                   + '99'
019400060606     c                                   + %subst(§b4ipn:4:8)
019500060606     c                   else
019600060606     c                   eval      w014a = §b4ipn
019700060606     c                   endif
019800060606     c                   endif
019900060606     c                   eval      laoope  = w014a
020000060606     c                                     + '_'
020100060606     c                                     + wtasdcm
020200040721     c                   WRITE     TILAO000
020300040721x   1c                   else
020400040721     c                   UPDATE    TILAO000
020500040721e   1c                   endif
020600040721      *
020700040721     c                   ENDSR
