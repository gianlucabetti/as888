000100090224     h DECEDIT('0,') DATEDIT(*YMD.)
000200980323     H*------------------------------------------------------------------------*
000300090224     H* ottimizzazione  tariffe/offerte tariffe particolari  da ftc = a        *
000301090224     H* se importo tariffa / minimo / massimo = 0 porto il tipo pagamento a 2  *
000400980323     H*------------------------------------------------------------------------*
000500980323     F*
000600090224     FTIOPT01L  Uf   e           K DISK
000700090224     FTIOPD01L  if   e           K DISK
001000030708     f
001001090224      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
001002090224     d $EoF            s              1    inz(*off)
001003090224     d $Err            s              1    inz(*off)
001004090224      * - Dati di TIOPT00F estratti via SQL
001005090224     d TIOPTds       e ds                  extname(TIOPT00F)  inz
001006090224      *
001007090224     C*
001008090224     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
001009090224     C
001010090224     C/EXEC SQL
001011090224     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
001012090224     C/END-EXEC
001013090224     C*
001014090224      *
001015090224      * - TIOPD01L
001016090224     c     K04Opd01      klist
001017090224     c                   kfld                    TPTksc
001018090224     c                   kfld                    TPTctr
001019090224     c                   kfld                    TPTprg
001020090224     c                   kfld                    TPTftc
001021090224      * Operazioni Iniziali
001022090224     c                   exsr      OpenCursor
001023090224      *
001024090224      * Ciclo di elaborazione
001025090224do  1c                   dou       $Eof  = *on
001026090224     c                   exsr      ReadCursor
001027090224e   1c                   enddo
001028090224      *
001029090224      * Fine programma
001030090224     c                   exsr      CloseCursor
001031090224     c                   eval      *inLR = *on
001032090224      *
001033090224      *****************************************************************
001034090224      *****************************************************************
001035090224      **?Apertura cursore.                                          ?**
001036090224      *****************************************************************
001037090224     c     OpenCursor    BEGSR
001038090224      *
001039090224     c/EXEC SQL
001040090224     c+ declare A1 cursor for
001041090224     c+ select  *
001042090224     c+ from    TIOPT00F
001043090224     c+ where   TPTFTC =  'a '
001044090224     c+   and   TPTTPG <> '2'
001045090224     c+ order   by tptksc, tptctr, tptprg
001046090224     c+ for     fetch only
001047090224     c/END-EXEC
001048090224      *
001049090224     c/EXEC SQL
001050090224     c+ open A1
001051090224     c/END-EXEC
001052090224      *
001053090224     c                   ENDSR
001054090224
001055090224      *****************************************************************
001056090224      **?Lettura cursore.                                           ?**
001057090224      *****************************************************************
001058090224     c     ReadCursor    BEGSR
001059090224      *
001060090224     c                   clear                   tioptds
001061090224      *
001062090224     c/EXEC SQL
001063090224     c+ fetch next from A1 into :tioptds
001064090224     c/END-EXEC
001065090224      *
001066090224sel 1c                   select
001067090224w   1c                   when      SQLcod = 100
001068090224     c                   eval      $EoF   = *on
001069090224w   1c                   when      SQLcod < 0
001070090224     c                   eval      $Err   = *on
001071090224     c                   eval      $Eof   = *on
001072090224x   1c                   other
001073090224     c                   exsr      ElabRec
001074090224e   1c                   endsl
001075090224      *
001076090224     c                   ENDSR
001077090224
001078090224      *****************************************************************
001079090224      **?Chiusura cursore.                                          ?**
001080090224      *****************************************************************
001081090224     c     CloseCursor   BEGSR
001082090224      *
001083090224     c/EXEC SQL
001084090224     c+ close A1
001085090224     c/END-EXEC
001086090224      *
001087090224     c                   ENDSR
001088090224
001089090224      *****************************************************************
001090090224      **?Elaborazione singolo record                                ?**
001091090224      *****************************************************************
001092090224     c     ElabRec       BEGSR
001093090224      *
001094090224     c                   clear                   maggiore
001095090224     c     k04opd01      setll     tiopd01l
001096090224     c                   do        *hival
001097090224     c     k04opd01      reade     tiopd01l
001098090224     c                   if        %eof(tiopd01l)
001099090224     c                   leave
001100090224     c                   endif
001101090224      * se c'è almeno un importo maggiore di zero non modifico il tipo
001102090224      * pagamento su titpt
001103090224     c                   add       tpditr        maggiore         15 3
001104090224     c                   add       tpdmin        maggiore
001105090224     c                   add       tpdmax        maggiore
001106090224     c                   enddo
001107090224      *
001108090224     c                   If        maggiore = 0
001109090224     c     k04opd01      chain     tiopt01l
001110090224     c                   if        %found(tiopt01l)
001111090224     c                   eval      tpttpg = '2'
001112090224     c                   clear                   tptarl
001113090224     c                   clear                   tptarf
001114090224     c                   clear                   tptaro
001115090224     c                   clear                   tptrpv
001116090224     c                   eval      tptduv = *date
001117090224     c                   update    titpt000
001118090224     c                   endif
001119090224     c                   endif
001200090224
001300090224     c                   endsr
