000100080812     h DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200980323     H*------------------------------------------------------------------------*
000300080812     H* PREP. FILE WORK WFAUT00F PER AUMENTO TARIFFE FUEL 2008                 *
000400031103     H*                                                                        *
000500031103     H*                                                                        *
000600980323     H*------------------------------------------------------------------------*
000700980323     F*
000800080812     Ftntam04l  if   e           k DISK
000900080812     Ftitpd01l  if   e           k DISK
001000080812     FWfafu01l  uf   e           k DISK
001100990614      *
001200031103
001300080812      *------------------------------------------------------------------------*
001400080812      *   V A R I A B I L I
001500080812      *------------------------------------------------------------------------*
001600080812     d kksc            s                   like(tamksc)
001700080814     d kftc            s                   like(tpdftc) inz('f ')
001800080813     d aumento         s              1
001900080901     d fuel            s              1
002000080902     d scaduta         s              1
002100080813
002200080812      *------------------------------------------------------------------------*
002300080812      *   D S   I N T E R N E / E S T E R N E
002400080812      *------------------------------------------------------------------------*
002500080812
002600080812     D PARAM           DS
002700080814     D  wksc                   1      7  0
002800080814     D  wctr                   8     10  0
002900080812
003000080812     d kpjba         e ds
003100080812     d dsta01        e ds
003200080812
003300031103      *------------------------------------------------------------------------*
003400031103
003500031103      *
003600080812    1c                   do        *hival
003700031103      *
003800080812     c                   read      Wfafu01l
003900031103      *
004000080812     c                   if        %eof(Wfafu01l)
004100031103     c                   leave
004200031103     c                   endif
004300031103      *
004400080812      * Per ogni cliente vado a cercare se ha tariffe
004500031103      *
004600080812     c                   clear                   wksc
004700080812     c                   eval      wctr = 999
004800080812     c                   move      wfaclf        kksc
004900080812
005000080812     c     kksc          setll     tntam04l
005100080812    2c                   do        *hival
005200080812     c     kksc          reade     tntam04l
005300080812      * fine file
005400080812     c                   if        %eof(tntam04l)
005500080812     c                   leave
005600080812     c                   endif
005700080812
005800080812      * cambio tariffa
005900080812    3c                   if        tamctr <> wctr or tamksc <> wksc
006000080812
006100080812     c                   movel     tamflo        dsta01
006200080812
006300080812      * escludo le annullate
006400080812     c                   if        tamatb <> *blanks
006500080812     c                   iter
006600080812     c                   endif
006700080812      * escludo le tariffe  FED
006800080812     c                   if        §tafed = 'S'
006900080812     c                   iter
007000080812     c                   endif
007100080902      * verifico se tariffa già scaduta
007200080902     c                   If        tamdst <= 20080930
007300080902     c                   eval      scaduta = 'S'
007400080902     c                   else
007500080902     c                   eval      scaduta = ' '
007600080902     c                   endif
007700080902      *
007800080812     c                   eval      aumento = ' '
007900080902     c                   eval      fuel    = ' '
008000080814      * mi salvo la tariffa
008100080814     c                   eval      wksc = tamksc
008200080814     c                   eval      wctr = tamctr
008300080814
008400080814    3c                   endif
008500080814
008600080902      * se tariffa sempre la stessa ma ho già trovato il fuel non elaboro tutti i progressivi
008700080902    3c                   if        tamctr = wctr and tamksc = wksc
008800080902     c                             and fuel = 'S'
008900080902     c                   iter
009000080902     c                   endif
009100080902
009200080814    3c                   If        aumento = ' '
009300080812      * cerco importo percentuale
009400080812     c     ktpt          setll     titpd01l
009500080812      * se non trovato il record lancio l'elaborazione della tariffa
009600080812     c                   if        not %equal(titpd01l)
009700080812     c                   eval      aumento = 'S'
009800080812     c                   endif
009900080812
010000080814    4c                   dow       aumento = ' '
010100080812     c     ktpt          reade     titpd01l
010200080812      * fine file
010300080812     c                   if        %eof(titpd01l)
010400080812     c                   leave
010500080812     c                   endif
010600080812      * non annullate
010700080812     c                   if        tpdatb <> *blanks
010800080812     c                   iter
010900080812     c                   endif
011000080812      * se tariffa trovata verifico importo in tariffa se minore di 0,3 aumento
011100080812     c                   If        tpditr < 0,3
011200080812     c                   eval      aumento = 'S'
011300080902     c                   leave
011400080902     c                   else
011500080902      * se tariffa del cliente già scaduta  passo alla tariffa successiva
011600080902     c                   if        scaduta = 'S'
011700080902     c                   eval      fuel = 'S'
011800080902     c                   leave
011900080902     c                   endif
012000080902      * se tariffa trovata e f.d. > = 0,3 e tariffa decorrente passo alla tariffa successiva
012100080902     c                   if        tamddt <= 20081001 and tamdst > 20081001
012200080902     c                   eval      fuel = 'S'
012300080902     c                   leave
012400080902     c                   endif
012500080812     c                   endif
012600080814    4c                   enddo
012700080812
012800080812      * se devo aumentare richiamo il pgm che aggiorna
012900080812     c                   If        aumento = 'S'
013000080812     c                   movel(p)  param         kpjbu
013100080812     c                   call      'TNVRAF3R'
013200080812     c                   parm                    kpjba
013300080812     c                   endif
013400080812
013500080812    3c                   endif
013600080812
013700080812    2c                   enddo
013800080812
013900080812     c                   eval      wfaela = 'S'
014000031113      *
014100080813     c                   update    wfafu000
014200031103      *
014300080812    1c                   enddo
014400031103      *
014500031103     c                   seton                                        lr
014600031103      *
014700080812      *--------------------------------------------------------------*
014800080812      * ROUTINE INIZIALE
014900080812      *--------------------------------------------------------------*
015000080812     c     *inzsr        begsr
015100080812
015200080812     c     *entry        plist
015300080812     c                   parm                    kpjba
015400080812
015500080812
015600080812     c     ktpt          klist
015700080812     c                   kfld                    tamksc
015800080812     c                   kfld                    tamctr
015900080812     c                   kfld                    tamprg
016000080812     c                   kfld                    kftc
016100080812
016200080812
016300080812
016400080812     c                   endsr
016500080812      *--------------------------------------------------------------*
