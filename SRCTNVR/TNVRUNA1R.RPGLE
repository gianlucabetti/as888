000100020318     H DECEDIT('0,') DATEDIT(*DMY/) option(*nodebugio)
000200020318
000300020318     fexunana01lip   e           k disk    rename(unana000:unanaold)  prefix(o_)
000400020320     fexunmov01lif   e           k disk    rename(unmov000:unmovold)  prefix(o_)
000500020318     funana01l  o  a e             disk
000600020320     funmov01l  o  a e             disk
000700020318     fqsysprt   o    f  132        printer oflind(*inof)
000800020318
000900020318     d trul33ds      e ds
001000020318     d kpjba         e ds
001100020318
001200020318     c     *entry        plist
001300020318     c                   parm                    kpjba
001400020320
001500020320     c     kunmov        klist
001600020320     c                   kfld                    o_unatip
001700020320     c                   kfld                    o_unacod
001800020318
001900020318     c                   clear                   unana000
002000020318     c                   eval      unatip = o_unatip
002100020318     c                   eval      unamar = o_unamar
002200020318     c                   eval      unamod = o_unamod
002300020318     c                   eval      unamat = o_unamat
002400020318     c                   eval      unades = o_unades
002500020318     c                   eval      unanot = o_unanot
002600020318     c                   eval      unadcn = o_unadcn
002700020318     c                   eval      unadsg = o_unadsg
002800020318     c                   eval      unasis = o_unasis
002900020318     c                   eval      unadms = o_unadms
003000020318     c                   eval      unadmf = o_unadmf
003100020318     c                   eval      unadsm = o_unadsm
003200020318     c                   eval      unalea = o_unalea
003300020318     c                   eval      unadsl = o_unadsl
003400020318     c                   eval      unamag = o_unamag
003500020318     c                   eval      unaemu = o_unaemu
003600020318     c                   eval      unamvd = o_unamvd
003700020318     c                   eval      unamta = o_unamta
003800020318     c                   eval      unamdf = o_unamdf
003900020318     c                   eval      unamra = o_unamra
004000020318     c                   eval      unamdr = o_unamdr
004100020318     c                   eval      unax25 = o_unax25
004200020318     c                   eval      unalsw = o_unalsw
004300020318     c                   eval      unaftr = o_unaftr
004400020318     c                   eval      unadtr = o_unadtr
004500020318     c                   eval      unaatb = o_unaatb
004600020318
004700020318      * Rinumero
004800020318     c                   clear                   trul33ds
004900020318     c                   eval      i33cnu = 22
005000020318     c                   eval      i33num = 1
005100020318     c                   movel(p)  trul33ds      kpjbu
005200020318     c                   call      'TRUL33R'
005300020318     c                   parm                    kpjba
005400020318     c                   movel(p)  kpjbu         trul33ds
005500020318      * stampo nuovo codice e scrivo file
005600020318     c                   if        o33err = *zeros
005700020318     c                   eval      unacod = o33nri
005800020318     c                   if        not *in99 or *inof
005900020318     c                   except    testa
006000020318     c                   eval      *in99 = *on
006100020318     c                   endif
006200020318     c                   except    riga
006300020320     c                   exsr      sr_unmov
006400020318     c                   write     unana000
006500020318     c                   endif
006600020318      * stampo errore ma non scrivo il file
006700020318     c                   if        o33err <> *zeros
006800020318     c                   if        not *in98 or *inof
006900020318     c                   except    testaerr
007000020318     c                   eval      *in98 = *on
007100020318     c                   endif
007200020318     c                   movel(p)  o33msg        errore           49
007300020318     c                   except    rigaerr
007400020318     c                   endif
007500020318
007600020318     c                   if        *inlr
007700020318     c                   clear                   trul33ds
007800020318     c                   eval      i33tla = 'C'
007900020318     c                   movel(p)  trul33ds      kpjbu
008000020318     c                   call      'TRUL33R'
008100020318     c                   parm                    kpjba
008200020318     c                   endif
008300020320
008400020320      *----------------------------------------------------------------
008500020320     c     sr_unmov      begsr
008600020320
008700020320     c     kunmov        setll     exunmov01l
008800020320     c                   do        *hival
008900020320     c     kunmov        reade     exunmov01l
009000020320     c                   if        %eof
009100020320     c                   leave
009200020320     c                   endif
009300020320     c                   clear                   unmov000
009400020320     c                   eval      unmcod = unacod
009500020320     c                   eval      unmtip = o_unmtip
009600020320     c                   eval      unmcau = o_unmcau
009700020320     c                   eval      unmtmo = o_unmtmo
009800020320     c                   eval      unmdtm = o_unmdtm
009900020320     c                   eval      unmprg = o_unmprg
010000020320     c                   eval      unmmag = o_unmmag
010100020320     c                   eval      unmmgc = o_unmmgc
010200020320     c                   eval      unmdes = o_unmdes
010300020320     c                   eval      unmdoc = o_unmdoc
010400020320     c                   eval      unmddo = o_unmddo
010500020320     c                   eval      unmcnt = o_unmcnt
010600020320     c                   eval      unmftr = o_unmftr
010700020320     c                   eval      unmdtr = o_unmdtr
010800020320     c                   eval      unmatb = o_unmatb
010900020320     c                   write     unmov000
011000020320     c                   enddo
011100020320
011200020320     c                   endsr
011300020318
011400020318     oqsysprt   e            testa            02
011500020318     o                                         +  5 '*** Elenco Unità EDP'
011600020318     o                                         +  1 'rinumerate'
011700020318     o                                         + 10 'TNVRUNA1R'
011800020318     o                       udate         y   + 15
011900020318     o                                          128 'Pag.'
012000020318     o                       page          z    132
012100020318     o          e            testa            04
012200020318     o                                           14 'Vecchio codice'
012300020318     o                                           27 'Nuovo codice'
012400020318     o                                           32 'Tipo'
012500020318     o                                           40 'Modello'
012600020318     o                                           58 'Matricola'
012700020318     o                                           76 'Descrizione'
012800020318     o          e            riga        1
012900020318     o                       o_unacod      z     10
013000020318     o                       unacod        z     25
013100020318     o                       unatip        z     31
013200020318     o                       unamod              48
013300020318     o                       unamat              64
013400020318     o                       unades              95
013500020318     o          e            testaerr         02
013600020318     o                                         +  5 '*** Elenco Unità EDP'
013700020318     o                                         +  1 'non rinumerate'
013800020318     o                                         + 10 'TNVRUNA1R'
013900020318     o                       udate         y   + 15
014000020318     o                                          128 'Pag.'
014100020318     o                       page          z    132
014200020318     o          e            testaerr         04
014300020318     o                                            4 'Tipo'
014400020318     o                                           19 'Vecchio codice'
014500020318     o                                           27 'Modello'
014600020318     o                                           45 'Matricola'
014700020318     o                                           63 'Descrizione'
014800020318     o          e            rigaerr     1
014900020318     o                       unatip        z      3
015000020318     o                       o_unacod      z     15
015100020318     o                       unamod              35
015200020318     o                       unamat              51
015300020318     o                       unades              82
015400020318     o                       errore             132
