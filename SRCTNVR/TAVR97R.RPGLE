000100990614     h DECEDIT('0,') DATEDIT(*DMY.)
000200980323     H*------------------------------------------------------------------------*
000300040302     H* PULIZIA DATI TIPO                                                      *
000400980323     H*------------------------------------------------------------------------*
000500980323     F*
000600040304     FTEETC00f  iF   e             DISK
000700990721     FTNTAM01l  if   e           k DISK
000701040304     FTNvis05l  if   e           k DISK    usropn
000800001122     fPRTF198   O    F  132        PRINTER  oflind(*INOF)
000900990721      *
001000040302     d savksc          s                   like(ETCksc)
001100040302     d savctr          s                   like(ETCctr)
001200040302     d savprg          s                   like(ETCprg)
001300990721      *
001400990721     c     keytam        klist
001500040302     c                   kfld                    ETCksc
001600040302     c                   kfld                    ETCctr
001700040302     c                   kfld                    ETCprg
001800001122      *
001801040304     c                   open      tnvis05l                             99
001802040304     C                   DO        *HIVAL
001803040304     C                   READ      TEETC00F
001804040304     C                   IF        %EOF(TEETC00F)
001805040304     C                   LEAVE
001806040304     C                   END
001900001122      * ogni volta che cambia la chiave aggancio tntam
002000040302     c                   if        savksc <> ETCksc or savctr <> ETCctr or
002100040302     c                             savprg <> ETCprg
002200040302     c                   eval      savksc = ETCksc
002300040302     c                   eval      savctr = ETCctr
002400040302     c                   eval      savprg = ETCprg
002500990614      *
002600990820      * aggancio la testata
002601040304     C                   IF        ETcCTO = 'T'
002602040304     c     keytam        chain     tntam01l                           30
002603040304     c                   end
002604040304     C                   IF        ETcCTO = 'O'
002605040304     c     etcksc        chain     tnvis05l                           30
002606040304     c                   end
002800001122     c                   if        not *in90 or *inof
002900001122     c                   except    testa
003000001122     c                   seton                                        90
003100001122     c                   setoff                                       of
003200001122     c                   endif
003300001122      * STAMPO GLI ERRORI
003301040304     c                   if        (not *in30 and tamatb = 'A'
003302040304     c                             and ETcatb = ' ' and etccto = 'T')
003500001122     c                             or *in30
003600040302     c                   except    detta
003700001122     c                   endif
003800001122      *
003900001122      *
004000001122     c                   endif
004100001122      *
004101040304     c                   if        etccto = 'T'
004200040302     c                   if        (not *in30 and tamatb = 'A' and ETCatb = ' ')
004300001123     c                              or *in30
004400040302      * se esiste la testata ma è annullata annullo anche il record dettaglio tariffario
004500040302     c                   eval      ETCatb = 'A'
004600040302     c                   clear                   ETCftr
004700040302     c                   clear                   ETCdtr
004800040302     c*!!*               update    TEETC000
004900001122     c                   endif
004901040304     c                   endif
005000001122      * se non esiste deleto il record
005001040304     c                   if        etccto = 'O'
005100001123     c*!!*               if        *in30
005200040302     c*!!*               delete    TEETC000
005300001123     c*!!*               endif
005301040304     c                   endif
005302040304     c                   endDO
005303040304     C                   SETON                                        LR
005400001123      *
005500001123      *------------------------------------------------------------
005600001122     OPRTF198   E            testa            01
005700040302     O                                        +   1 'DATI TIPO (TEETC'
005800040302     O                                              ') senza testata o con tes'
005900001122     o                                              'tata annullata'
006000001122     OPRTF198   E            testa            03
006100040304     O                                        +   0 'S/F'
006200040304     O                                        +   1 'O/T Cli/Vis tariffa  prog'
006300040304     O                                        +   1 'Anomalia'
006400040304     O          E            detta       1
006500040304     O                       ETcDSF           +   1
006600040304     O                       ETcCTO           +   3
006700040304     O                       ETcksc           +   2
006800040304     O                       ETcctr           +   3
006900040304     O                       ETcprg           +   4
007000040304     O               30                          51 'Non Esiste la tar/vis'
007100040304     O              N30                          51 'Tar/Vis Annullata    '
