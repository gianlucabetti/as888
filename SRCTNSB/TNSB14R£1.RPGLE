000100040721      *===============================================================*
000200040721      * TNSB10R    * Aggiornamento TITAS per immagine LdV             *
000300040721      *===============================================================*
000400040721
000500040721     h Decedit('0,') Datedit(*dmy/) Option(*nodebugio)
000600040721
000700040721      *===============================================================*
000800040721
000900041021     fTITAS26C  if   e           k disk
001000040721     fTILAO01L  uf a e           k disk
001100040721
001200040721      *===============================================================*
001300040721
001400040721      * Strutture Dati - - - - - - - - - - - - - - - - - - - - - - - -*
001500040721      *
001600040721     d KPJBA         e ds
001700040721      * - Parametri x Controllo profilo utenti
001800040721     d TIBS34DS      e ds                  inz
001900040721      * - Ds di riferimento al file esterno AZUTE00F
002000040721     d AZUTEDS       e ds                  extname(AZUTE00F)
002100040721      * - Ds per dati organigramma
002200040721     d DDatiUte      e ds
002300040721      * - Ds per campo TASFLO del file TITAS30C
002400040721     d dTASflo       e ds                  inz
002500040721      *
002600040721     d Status         sds           333
002700040721     d   SDSpgm          *proc
002800040721     d   SDSjob              244    253                                         Job name
002900040721     d***SDSusr              254    263                                         User name
003000040721     d***SDSjnr              264    269S 0                                      Job number
003100040721      *
003200040721     d TNSB14ds        ds                  inz
003300040721     d   D14dcd                       8  0 inz
003400040721     d   D14dca                       8  0 inz
003500040721      *
003600040721     d WLBdat          ds                  inz
003700040721     d   G08dat                       8  0 inz
003800040721     d   G08inv                       8  0 inz
003900040721     d   G08err                       1    inz(*off)
004000040721     d   G08tgi                       5  0 inz
004100040721      *
004200040721     d DS_Time1        ds            14    inz
004300040721     d   dsT1_hms                     6  0 inz
004400040721     d   dsT1_dmy                     8  0 inz
004500040721     d DS_Time2        ds            14    inz
004600040721     d   dsT2_ymd                     8  0 inz
004700040721     d   dsT2_hms                     6  0 inz
004800040721      *
004900040721      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
005000040721      *
005100040721      * - Campi di comodo
005200040721     d WdateISO        s               d   datfmt(*ISO) inz
005300040721     d Wtime           s             14  0              inz
005400040721     d WtasDCM         s              8                 inz(*zeros)
005500040721     d WtasRMN         s             15                 inz(*zeros)
005600040721
005700040721      *===============================================================*
005800040721      *  RIEPILOGO INDICATORI                                         *
005900040721      *---------------------------------------------------------------*
006000040721      * 10    - Comodo                                                *
006100040721      *===============================================================*
006200040721
006300040721     c     *Entry        plist
006400040721     c                   parm                    KPJBA
006500040721     c                   movel     KPJBU         TNSB14ds
006600040721      *
006700040721      * Operazioni iniziali
006800040721     c                   exsr      OperazIniz
006900040721      *
007000041021      * Ciclo di lettura del file TITAS26C
007100041021     c     D14dcd        setll     TITAS26C
007200041021     c                   read      TITAS26C
007300040721      *
007400041021do  1c                   dow       NOT %eof(TITAS26C)
007500040721     c                             and TASdcm <= D14dca
007600040721      *
007700040721     c                   movel     TASflo        dTASflo
007800040721      * - Elaborazione delle sole LdV con immagine inviata a DPD
007900040721if  2c                   if        §FLOiml  = 'I'
008000040721     c                   exsr      Wrt_TILAO
008100040721e   2c                   endif
008200040721      *
008300041021      * - Lettura del record successivo nel file TITAS26C
008400041021     c                   read      TITAS26C
008500040721      *
008600040721e   1c                   enddo
008700040721      *
008800040721      * Fine
008900040721     c                   eval      *inLR = *on
009000040721      *
009100040721      *---------------------------------------------------------------*
009200040721      * Operazioni iniziali                                           *
009300040721      *---------------------------------------------------------------*
009400040721     c     OperazIniz    BEGSR
009500040721      *
009600040721      * Definizione Key-List
009700040721      * - TILAO01L
009800040721     c     K05LAO01      klist
009900040721     c                   kfld                    LAOaas
010000040721     c                   kfld                    LAOlnp
010100040721     c                   kfld                    LAOnrs
010200040721     c                   kfld                    LAOnsp
010300040721     c                   kfld                    LAOcop
010400040721      *
010500040721      * Reperisco dati job
010600040721     c                   exsr      DatiJob
010700040721      *
010800040721      * Impostazione del campo LAOTIM = aaaa/mm/gg+hh:mm:ss
010900040721     c                   time                    Wtime
011000040721     c                   movel     Wtime         DS_Time1
011100040721     c                   eval      dsT2_hms   = dsT1_hms
011200040721     c     *eur          movel     dsT1_dmy      WdateISO
011300040721     c                   move      WdateISO      dsT2_ymd
011400040721      *
011500040721     c                   ENDSR
011600040721      *
011700040721      *---------------------------------------------------------------*
011800040721      * Reperimento Dati del job (Utente/Operativi)                   *
011900040721      *---------------------------------------------------------------*
012000040721     c     DatiJob       BEGSR
012100040721      *
012200040721     c     *dtaara       define    §azute        azuteds
012300040721     c     *dtaara       define    §datiute      ddatiute
012400040721      *
012500040721     c                   in(E)     *dtaara
012600040721     c                   IF        %ERROR or RSUT = *blanks
012700040721     c                   clear                   Tibs34Ds
012800040721     c                   call      'TIBS34R'
012900040721     c                   parm                    Tibs34Ds
013000040721     c                   in        *dtaara
013100040721     c                   ENDIF
013200040721      *
013300040721     c                   ENDSR
013400040721      *
013500040721      *---------------------------------------------------------------*
013600040721      * Registrazione dati nel file TILAO00F x DPD                    *
013700040721      *---------------------------------------------------------------*
013800040721     c     Wrt_TILAO     BEGSR
013900040721      *
014000040721      * Impostazione dei campi numerici in campi alfanumerici di comodo
014100040721     c                   movel     TASdcm        WtasDCM
014200040721     c                   move      TASrmn        WtasRMN
014300040721      *
014400040721      * Aggancio record corrispondente nel file TILAO00F
014500040721     c                   clear                   TILAO000
014600040721     c                   move      TASaas        LAOaas
014700040721     c                   move      TASlnp        LAOlnp
014800040721     c                   move      TASnrs        LAOnrs
014900040721     c                   move      TASnsp        LAOnsp
015000040721     c                   movel     'DPD'         LAOcop
015100040721     c     K05LAO01      chain     TILAO000
015200040721      *
015300040721      * Impostazione nuovi dati
015400040721     c                   eval      LAOtim  = DS_Time2
015500040721     c                   eval      LAGidl  = SDSjob
015600040721     c                   eval      LAGela  = *zeros
015700040721      *
015800040721      * Aggiornamento record
015900040721if  1c                   if        NOT %found(TILAO01L)
016000040721     c***                clear                   LAOxx1
016100040721     c                   eval      LAOope  = %subst(wTASrmn:4:11)
016200040721     c                                     + '_'
016300040721     c                                     + wTASdcm
016400040721     c                   WRITE     TILAO000
016500040721x   1c                   else
016600040721     c                   UPDATE    TILAO000
016700040721e   1c                   endif
016800040721      *
016900040721     c                   ENDSR
