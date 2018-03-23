000100990614     h DECEDIT('0,') DATEDIT(*DMY.)
000200980323     H*------------------------------------------------------------------------*
000300040302     H* PULIZIA codici tassazione                                              *
000400980323     H*------------------------------------------------------------------------*
000500980323     F*
000600040304     FTEETT00f  iF   e             DISK
000700990721     FTNTAM01l  if   e           k DISK
000701040304     FTNvis05l  if   e           k DISK    USROPN
000800001122     fPRTF198   O    F  132        PRINTER  oflind(*INOF)
000900990721      *
001000040302     d savksc          s                   like(ETTksc)
001100040302     d savctr          s                   like(ETTctr)
001200040302     d savprg          s                   like(ETTprg)
001300990721      *
001400990721     c     keytam        klist
001500040302     c                   kfld                    ETTksc
001600040302     c                   kfld                    ETTctr
001700040302     c                   kfld                    ETTprg
001800001122      *
001801040304     c                   open      tnvis05l                             99
001802040304     C                   DO        *HIVAL
001803040304     C                   READ      TEETT00F
001804040304     C                   IF        %EOF(TEETT00F)
001805040304     C                   LEAVE
001806040304     C                   END
001900001122      * ogni volta che cambia la chiave aggancio tntam
002000040302     c                   if        savksc <> ETTksc or savctr <> ETTctr or
002100040302     c                             savprg <> ETTprg
002200040302     c                   eval      savksc = ETTksc
002300040302     c                   eval      savctr = ETTctr
002400040302     c                   eval      savprg = ETTprg
002500990614      *
002600990820      * aggancio la testata
002601040304     C                   IF        ETTCTO = 'T'
002700990820     c     keytam        chain     tntam01l                           30
002701040304     c                   end
002702040304     C                   IF        ETTCTO = 'O'
002703040304     c     ettksc        chain     tnvis05l                           30
002704040304     c                   end
002800001122     c                   if        not *in90 or *inof
002900001122     c                   except    testa
003000001122     c                   seton                                        90
003100001122     c                   setoff                                       of
003200001122     c                   endif
003300001122      * STAMPO GLI ERRORI
003400040304     c                   if        (not *in30 and tamatb = 'A'
003401040304     c                             and ETTatb = ' ' and ettcto = 'T')
003500001122     c                             or *in30
003600001122     c                   except    detta
003700001122     c                   endif
003800001122      *
004000001122     c                   endif
004100001122      *
004101040304     c                   if        ettcto = 'T'
004200040302     c                   if        (not *in30 and tamatb = 'A' and ETTatb = ' ')
004300001123     c                              or *in30
004400001122      * se esiste la testata ma è annullata annullo anche il record dettaglio tariffario
004500040302     c                   eval      ETTatb = 'A'
004600040302     c                   clear                   ETTftr
004700040302     c                   clear                   ETTdtr
004800040302     c*!!*               update    TEETT000
004900001122     c                   endif
004901040304     c                   endif
005000001122      * se non esiste deleto il record
005001040304     c                   if        ettcto = 'O'
005100001123     c*!!*               if        *in30
005200040302     c*!!*               delete    TEETT000
005304040304     c*!!*               endif
005305040304     c                   endif
005400001123      *
005401040304     c                   endDO
005402040304     C                   SETON                                        LR
005500001123      *------------------------------------------------------------
005600001122     OPRTF198   E            testa            01
005700040302     O                                        +   1 'CODICI TASSAZIONE (TEETT'
005800040302     O                                              ') senza testata o con tes'
005900001122     o                                              'tata annullata'
006000001122     OPRTF198   E            testa            03
006100040304     O                                        +   0 'S/F'
006101040304     O                                        +   1 'O/T Cli/Vis tariffa  prog'
006200040304     O                                        +   1 'Anomalia'
006300001122     O          E            detta       1
006400040302     O                       ETTDSF           +   1
006401040304     O                       ETTCTO           +   3
006402040304     O                       ETTksc           +   2
006500040302     O                       ETTctr           +   3
006600040302     O                       ETTprg           +   4
006700040304     O               30                          51 'Non Esiste la tar/vis'
006800040304     O              N30                          51 'Tar/Vis Annullata    '
