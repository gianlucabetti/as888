000100041230     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200130415      *----------------------------------------------------------------*
000300130415      * Pulisco cliente dai reclami se non presenti in CNACO (Gallucci)*
000400130415      *----------------------------------------------------------------*
000500130415     fFITGD00F  if   e             disk    rename(fitgd000:fitgd0) prefix(F_)
000600130415     fFITGD01L  uf   e           k disk
000700041230      *
000800041230      *
000900041231      * - Parametri x Controllo profilo utenti
001000041231     d TIBS34DS      e ds                  inz
001100041231      * - Ds di riferimento al file esterno AZUTE00F
001200041231     d AZUTEDS       e ds                  extname(AZUTE00F)
001300041231      * - Ds per dati organigramma
001400041231     d DDatiUte      e ds
001500130415      * - Ds per Anagrafiche Clienti
001600130415     d TIBS69DS      E DS
001700130415     d DS_cnaco      E DS                  extname(CNACO00F)
001800130415     d DS_cnind      E DS                  extname(CNIND00F)
001900130415     d DS_cnclp      E DS                  extname(CNCLP00F)
002000130415     d DS_fncls      E DS                  extname(FNCLS00F)
002100041230      *
002200041231     d KPJBA         e ds
002300041230      *
002400041231     c                   do        *hival
002500041231     c                   read      fitgd00f
002600041231     c                   if        %eof(fitgd00f)
002700041231     c                   leave
002800041231     c                   endif
002900130415      * se cliente non valorizzato leggo il successivo
003000130415     c                   If        F_tgdksc =  *zeros
003100041231     c                   iter
003200041231     c                   endif
003300041231
003400130415
003500130415     C                   clear                   TIBS69DS
003600130415     c                   z-add     F_tgdksc      I69kac
003700130415     C                   call      'TIBS69R'
003800130415     C                   parm                    tibs69DS
003900130415     C                   parm                    DS_cnaco
004000130415     C                   parm                    DS_cnind
004100130415     C                   parm                    DS_cnclp
004200130415     C                   parm                    DS_fncls
004300130415     c                   If         O69ERR =  '1'
004400130415     c     Ktgdan        chain(e)  fitgd01l
004500130415     c                   if        not %error
004600130415     C                   clear                   tgdksc
004700130415     c                   update    Fitgd000
004800130415     c                   Endif
004900130415     c                   Endif
005000041230      *
005100041231
005200041231     c                   enddo
005300041231      *
005400130415     C                   eval      I69TLA  = 'C'
005500130415     C                   call      'TIBS69R'
005600130415     C                   parm                    TIBS69DS
005700130415      *
005800041231     c                   seton                                        lr
005900041230      *
006000041231      *---------------------------------------------------------------*
006100041231      * Routine Iniziale                                              *
006200041231      *---------------------------------------------------------------*
006300041231     c     *InzSR        BegSR
006400041231      *
006500041231     c     *Entry        plist
006600041231     c                   parm                    KPJBA
006700041231      *
006800041231      * Reperisco dati job
006900041231     c                   exsr      DatiJob
007000130415      * - file FITGD01L
007100130415     c     ktgdan        klist
007200130415     c                   kfld                    F_tgdant
007300130415     c                   kfld                    F_tgdnut
007400130415     c                   kfld                    F_tgdpdt
007500130415
007600041231
007700041231
007800041231     C                   ENDSR
007900041231      *
008000041231      *---------------------------------------------------------------*
008100041231      * Reperimento Dati del job (Utente/Operativi)                   *
008200041231      *---------------------------------------------------------------*
008300041231     c     DatiJob       BegSr
008400041231      *
008500041231     c     *dtaara       define    §azute        azuteds
008600041231     c     *dtaara       define    §datiute      ddatiute
008700041231      *
008800041231     c                   in(E)     *dtaara
008900041231     c                   IF        %ERROR or RSUT = *blanks
009000041231     c                   clear                   Tibs34Ds
009100041231     c                   call      'TIBS34R'
009200041231     c                   parm                    Tibs34Ds
009300041231     c                   in        *dtaara
009400041231     c                   ENDIF
009500041231     C                   ENDSR
