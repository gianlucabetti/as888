000100990614     h DECEDIT('0,') DATEDIT(*DMY.)
000200980323     H*------------------------------------------------------------------------*
000300040302     H* PULIZIA SCAGLIONI                                                      *
000400980323     H*------------------------------------------------------------------------*
000500980323     F*
000600040304     FTEETS00f  iF   e             DISK
000700990721     FTNTAM01l  if   e           k DISK
000701040304     FTNvis05l  if   e           k DISK    usropn
000800001122     fPRTF198   O    F  132        PRINTER  oflind(*INOF)
000900990721      *
001000040302     d savksc          s                   like(ETSksc)
001100040302     d savctr          s                   like(ETSctr)
001200040302     d savprg          s                   like(ETSprg)
001300990721      *
001400990721     c     keytam        klist
001500040302     c                   kfld                    ETSksc
001600040302     c                   kfld                    ETSctr
001700040302     c                   kfld                    ETSprg
001800001122      *
001801040304     c                   open      tnvis05l                             99
001802040304     C                   DO        *HIVAL
001803040304     C                   READ      TEETS00F
001804040304     C                   IF        %EOF(TEETS00F)
001805040304     C                   LEAVE
001806040304     C                   END
001900001122      * ogni volta che cambia la chiave aggancio tntam
002000040302     c                   if        savksc <> ETSksc or savctr <> ETSctr or
002100040302     c                             savprg <> ETSprg
002200040302     c                   eval      savksc = ETSksc
002300040302     c                   eval      savctr = ETSctr
002400040302     c                   eval      savprg = ETSprg
002500990614      *
002600990820      * aggancio la testata
002701040304     C                   IF        etsCTO = 'T'
002702040304     c     keytam        chain     tntam01l                           30
002703040304     c                   end
002704040304     C                   IF        etsCTO = 'O'
002705040304     c     etsksc        chain     tnvis05l                           30
002706040304     c                   end
002800001122     c                   if        not *in90 or *inof
002900001122     c                   except    testa
003000001122     c                   seton                                        90
003100001122     c                   setoff                                       of
003200001122     c                   endif
003300001122      * STAMPO GLI ERRORI
003401040304     c                   if        (not *in30 and tamatb = 'A'
003402040304     c                             and etsatb = ' ' and etscto = 'T')
003500001122     c                             or *in30
003600040302     c                   except    detta
003700001122     c                   endif
003800001122      *
003900001122      *
004000001122     c                   endif
004100001122      *
004101040304     c                   if        etscto = 'T'
004102040304     c                   if        (not *in30 and tamatb = 'A' and ETSatb = ' ')
004300001123     c                              or *in30
004400040302      * se esiste la testata ma è annullata annullo anche il record dettaglio tariffario
004500040302     c                   eval      ETSatb = 'A'
004600040302     c                   clear                   ETSftr
004700040302     c                   clear                   ETSdtr
004800040302     c*!!*               update    TEETS000
004900001122     c                   endif
004901040304     c                   endif
005000001122      * se non esiste deleto il record
005001040304     c                   if        etscto = 'O'
005100001123     c*!!*               if        *in30
005200040302     c*!!*               delete    TEETS000
005300001123     c*!!*               endif
005301040304     c                   endif
005302040304     c                   endDO
005303040304     C                   SETON                                        LR
005400001123      *
005500001123      *------------------------------------------------------------
005600001122     OPRTF198   E            testa            01
005700040302     O                                        +   1 'SCAGLIONI (TEETS'
005800040302     O                                              ') senza testata o con tes'
005900001122     o                                              'tata annullata'
006000001122     OPRTF198   E            testa            03
006100040304     O                                        +   0 'S/F'
006200040304     O                                        +   1 'O/T Cli/Vis tariffa  prog'
006300040304     O                                        +   1 'Anomalia'
006400040304     O          E            detta       1
006500040304     O                       etsDSF           +   1
006600040304     O                       ETsCTO           +   3
006700040304     O                       ETsksc           +   2
006800040304     O                       ETsctr           +   3
006900040304     O                       ETsprg           +   4
007000040304     O               30                          51 'Non Esiste la tar/vis'
007100040304     O              N30                          51 'Tar/Vis Annullata    '
