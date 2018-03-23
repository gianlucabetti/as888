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
001100080908     FQsysprt   O    F  132        printer oflind(*inof)
001200990614      *
001300031103
001400080812      *------------------------------------------------------------------------*
001500080812      *   V A R I A B I L I
001600080812      *------------------------------------------------------------------------*
001700080812     d kksc            s                   like(tamksc)
001800080814     d kftc            s                   like(tpdftc) inz('f ')
001900080813     d aumento         s              1
002000080901     d fuel            s              1
002100080902     d scaduta         s              1
002200080813
002300080812      *------------------------------------------------------------------------*
002400080812      *   D S   I N T E R N E / E S T E R N E
002500080812      *------------------------------------------------------------------------*
002600080812
002700080812     D PARAM           DS
002800080814     D  wksc                   1      7  0
002900080814     D  wctr                   8     10  0
003000080904     D  tipo_aumento          11     11
003100080812
003200080812     d kpjba         e ds
003300080812     d dsta01        e ds
003400080812
003500031103      *------------------------------------------------------------------------*
003600031103
003700031103      *
003800080812    1c                   do        *hival
003900031103      *
004000080812     c                   read      Wfafu01l
004100031103      *
004200080812     c                   if        %eof(Wfafu01l)
004300031103     c                   leave
004400031103     c                   endif
004500031103      *
004600080812      * Per ogni cliente vado a cercare se ha tariffe
004700031103      *
004800080812     c                   clear                   wksc
004900080812     c                   eval      wctr = 999
005000080812     c                   move      wfaclf        kksc
005100080812
005200080812     c     kksc          setll     tntam04l
005300080812    2c                   do        *hival
005400080812     c     kksc          reade     tntam04l
005500080812      * fine file
005600080812     c                   if        %eof(tntam04l)
005700080812     c                   leave
005800080812     c                   endif
005900080812
006000080812      * cambio tariffa
006100080812    3c                   if        tamctr <> wctr or tamksc <> wksc
006200080812
006300080812     c                   movel     tamflo        dsta01
006400080812
006500080812      * escludo le annullate
006600080812     c                   if        tamatb <> *blanks
006700080812     c                   iter
006800080812     c                   endif
006900080812      * escludo le tariffe  FED
007000080812     c                   if        §tafed = 'S'
007100080812     c                   iter
007200080812     c                   endif
007300080902      * verifico se tariffa già scaduta
007400080902     c                   If        tamdst <= 20080930
007500080902     c                   eval      scaduta = 'S'
007600080902     c                   else
007700080902     c                   eval      scaduta = ' '
007800080902     c                   endif
007900080902      *
008000080812     c                   eval      aumento = ' '
008100080902     c                   eval      fuel    = ' '
008200080814      * mi salvo la tariffa
008300080814     c                   eval      wksc = tamksc
008400080814     c                   eval      wctr = tamctr
008500080814
008600080814    3c                   endif
008700080814
008800080902      * se tariffa sempre la stessa ma ho già trovato il fuel non elaboro tutti i progressivi
008900080902    3c                   if        tamctr = wctr and tamksc = wksc
009000080902     c                             and fuel = 'S'
009100080902     c                   iter
009200080902     c                   endif
009300080902
009400080814    3c                   If        aumento = ' '
009500080812      * cerco importo percentuale
009600080812     c     ktpt          setll     titpd01l
009700080812      * se non trovato il record lancio l'elaborazione della tariffa
009800080812     c                   if        not %equal(titpd01l)
009900080812     c                   eval      aumento = 'S'
010000080812     c                   endif
010100080812
010200080814    4c                   dow       aumento = ' '
010300080812     c     ktpt          reade     titpd01l
010400080812      * fine file
010500080812     c                   if        %eof(titpd01l)
010600080812     c                   leave
010700080812     c                   endif
010800080812      * non annullate
010900080812     c                   if        tpdatb <> *blanks
011000080812     c                   iter
011100080812     c                   endif
011200080908      * se tariffa con scaglione minore di 999 stampo la tariffa e non aumento
011300080908     c                   If        tpdsgl < 999
011400080908     c                   if        not *in99 or *inof
011500080908     c                   except    testa
011600080908     c                   seton                                        99
011700080908     c                   setoff                                       of
011800080908     c                   endif
011900080908
012000080908     c                   except    detta
012100080908     c                   eval      aumento = 'N'
012200080908     c                   leave
012300080908     c                   endif
012400080812      * se tariffa trovata verifico importo in tariffa se minore di 0,3 aumento
012500080812     c                   If        tpditr < 0,3
012600080812     c                   eval      aumento = 'S'
012700080902     c                   leave
012800080902     c                   else
012900080902      * se tariffa del cliente già scaduta  passo alla tariffa successiva
013000080902     c                   if        scaduta = 'S'
013100080902     c                   eval      fuel = 'S'
013200080902     c                   leave
013300080902     c                   endif
013400080902      * se tariffa trovata e f.d. > = 0,3 e tariffa decorrente passo alla tariffa successiva
013500080902     c                   if        tamddt <= 20081001 and tamdst > 20081001
013600080902     c                   eval      fuel = 'S'
013700080902     c                   leave
013800080902     c                   endif
013900080812     c                   endif
014000080814    4c                   enddo
014100080812
014200080812      * se devo aumentare richiamo il pgm che aggiorna
014300080812     c                   If        aumento = 'S'
014400080904     c                   eval      tipo_aumento =  wfatip
014500080812     c                   movel(p)  param         kpjbu
014600080812     c                   call      'TNVRAF3R'
014700080812     c                   parm                    kpjba
014800080812     c                   endif
014900080812
015000080812    3c                   endif
015100080812
015200080812    2c                   enddo
015300080812
015400080812     c                   eval      wfaela = 'S'
015500031113      *
015600080813     c                   update    wfafu000
015700031103      *
015800080812    1c                   enddo
015900031103      *
016000031103     c                   seton                                        lr
016100031103      *
016200080812      *--------------------------------------------------------------*
016300080812      * ROUTINE INIZIALE
016400080812      *--------------------------------------------------------------*
016500080812     c     *inzsr        begsr
016600080812
016700080812     c     *entry        plist
016800080812     c                   parm                    kpjba
016900080812
017000080812
017100080812     c     ktpt          klist
017200080812     c                   kfld                    tamksc
017300080812     c                   kfld                    tamctr
017400080812     c                   kfld                    tamprg
017500080812     c                   kfld                    kftc
017600080812
017700080812
017800080812
017900080812     c                   endsr
018000080812      *--------------------------------------------------------------*
018100080908      * Stampa
018200080908      *-------------------------------------------------------------*
018300080908     OQsysPrt   E            Testa            01
018400080908     o                                           66 'Tariffe che non vengono'
018500080908     o                                              ' aumentate'
018600080908     O          E            Testa          1 03
018700080908     o                                           12 'Cod.cliente '
018800080908     o                                          + 2 'Tariffa'
018900080908     o                                          + 2 'Prg'
019000080908     o                                          + 2 'Motivo '
019100080908     O          E            detta          1
019200080908     O                       tpdksc               8
019300080908     O                       tpdctr              18
019400080908     O                       tpdprg              26
019500080908     O                                          + 2 'Scaglione inferiore a 999'
