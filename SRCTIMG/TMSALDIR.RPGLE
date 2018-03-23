000100080124
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400991022     Ftmmoc15l  if   e           k disk
000500991022     Ftmsmp02l  if   e           k disk
000600991022     fsysprt    o    f  132        printer
000700080124     d dividi          s            120
000800000000     C*---------------------------------------------------------------*
000900991022     c     ksmp          klist
001000991022     c                   kfld                    anno              4 0
001100991022     c                   kfld                    exmocmat         12
001200090108     c                   z-add     2009          anno
001300080124     c                   move      *all'_'       dividi
001400991022     c                   exsr      tmmoc
001500990113     c                   seton                                        lr
001600990114     c*-------------------------------------------------
001700990114     c     tmmoc         begsr
001800990114     c*-------------------------------------------------
001900990114     c                   setoff                                           99
002000991022     c     *loval        setll     tmmoc15l
002100990114     c                   do        *hival
002200991022     c                   read      tmmoc15l                               99
002300990114     c   99              leave
002400991022     c                   if        pas = *blank
002500991022     c                   except    testa
002600991022     c                   move      mocmat        exmocmat         12
002700991022     c                   move      '1'           pas               1
002800991022     c                   end
002900991022     c     mocmat        comp      exmocmat                           0101
003000991022     c   01              exsr      tmsmp
003100991022     c                   select
003200991022     c                   when      mocfas = 'AS'
003300991022     c                   add       mocqta        qtaas            11 2
003400991022     c                   if        mocdmn > exmocdmn
003500991022     c                   z-add     mocdmn        exmocdmn          8 0
003600991022     c                   end
003700991022     c                   when      mocfas = 'OR'
003800991022     c                   add       mocqta        qtaor            11 2
003900991022     c                   if        mocdmn > exmocdmn
004000991022     c                   z-add     mocdmn        exmocdmn
004100991022     c                   end
004200991022     c                   endsl
004300990114     c                   enddo
004400990114     c                   endsr
004500991022     c*-------------------------------------------------
004600991022     c     tmsmp         begsr
004700991022     c*-------------------------------------------------
004800991022     c     ksmp          chain     tmsmp02l                           99
004900991022     c                   if        not *in99
005000991022     c                   if        qtaor <> smpqpr or
005100991022     c                             qtaas <> smpqim
005200991022     c                   except    saldo
005300991022     c                   z-add     0             qtaor
005400991022     c                   z-add     0             qtaas
005500991022     c                   z-add     0             exmocdmn
005600991022     c                   end
005700991022     c                   else
005800080124     c                   z-add     0             qtaor
005900080124     c                   z-add     0             qtaas
006000080124     c                   z-add     0             exmocdmn
006100080124     c*                  except    saldob
006200991022     c                   end
006300991022     c                   move      mocmat        exmocmat
006400080129     c                   z-add     0             qtaor
006500080129     c                   z-add     0             qtaas
006600080129     c                   z-add     0             exmocdmn
006700991022     c                   endsr
006800991022      *---------------------------------------------------------------------
006900991022     Osysprt    E            testa            01
007000991022     O                                           70 'Righe con saldi errati'
007100991022     O*
007200991022     O          E            testa       2
007300991022     o                                           14 'Codice'
007400991022     o                                           30 'Qta ordinata'
007500991022     o                                           50 'Qta assegnata'
007600080124     O          E            testa       1
007700080124     o                                           30 'Qta in Ord. '
007800080124     o                                           50 'Qta in Ass. '
007900080124     o                                           66 'Qta in Ass.P'
008000080124     o                                           82 'Qta in Or.Fo'
008100080124     o                                           94 'Qta Giac.Fi'
008200080124     o                                          106 'Magaz.'
008300991022     O          E            saldo       2
008400991022     o                       exmocmat            14
008500991022     o                       qtaor         2     30
008600991022     o                       qtaas         2     50
008700080124     o                       exmocdmn           120 '    /  /  '
008800991022     O          E            saldo       1
008900080124     o*                      smpmat              14
009000991022     o                       smpqpr        2     30
009100991022     o                       smpqim        2     50
009200991022     o                       smpqip        2     66
009300991022     o                       smpqor        2     82
009400080124     o                       smpqgf        2     94
009500080124     o                       smpmag             106
009600080124     o                       smpdmn             120 '    /  /  '
009700080124     O          E            saldo       1
009800080124     o                       dividi             120
009900080124     O          E            saldob      2
010000080124     o                       exmocmat            14
010100080124     o                       qtaor         2     30
010200080124     o                       qtaas         2     50
010300080124     o                                           80 'non trovato il saldo'
