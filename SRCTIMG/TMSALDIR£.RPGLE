000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
001300991022     Ftmmoc15l  if   e           k disk
001301991022     Ftmsmp02l  if   e           k disk
001302991022     fsysprt    o    f  132        printer
002300000000     C*---------------------------------------------------------------*
002301991022     c     ksmp          klist
002302991022     c                   kfld                    anno              4 0
002303991022     c                   kfld                    exmocmat         12
002304991022     c                   z-add     1999          anno
002305991022     c                   exsr      tmmoc
003700990113     c                   seton                                        lr
023600990114     c*-------------------------------------------------
023700990114     c     tmmoc         begsr
023800990114     c*-------------------------------------------------
023900990114     c                   setoff                                           99
024000991022     c     *loval        setll     tmmoc15l
024100990114     c                   do        *hival
024200991022     c                   read      tmmoc15l                               99
024300990114     c   99              leave
024301991022     c                   if        pas = *blank
024302991022     c                   except    testa
024303991022     c                   move      mocmat        exmocmat         12
024304991022     c                   move      '1'           pas               1
024305991022     c                   end
024400991022     c     mocmat        comp      exmocmat                           0101
024500991022     c   01              exsr      tmsmp
024600991022     c                   select
024700991022     c                   when      mocfas = 'AS'
024800991022     c                   add       mocqta        qtaas            11 2
024801991022     c                   if        mocdmn > exmocdmn
024802991022     c                   z-add     mocdmn        exmocdmn          8 0
024803991022     c                   end
024804991022     c                   when      mocfas = 'OR'
024805991022     c                   add       mocqta        qtaor            11 2
024806991022     c                   if        mocdmn > exmocdmn
024807991022     c                   z-add     mocdmn        exmocdmn
024808991022     c                   end
024809991022     c                   endsl
033300990114     c                   enddo
033400990114     c                   endsr
033500991022     c*-------------------------------------------------
033600991022     c     tmsmp         begsr
033700991022     c*-------------------------------------------------
033900991022     c     ksmp          chain     tmsmp02l                           99
033901991022     c                   if        not *in99
033902991022     c                   if        qtaor <> smpqpr or
033904991022     c                             qtaas <> smpqim
033905991022     c                   except    saldo
033906991022     c                   z-add     0             qtaor
033907991022     c                   z-add     0             qtaas
033908991022     c                   z-add     0             exmocdmn
033909991022     c                   end
033910991022     c                   else
033911991022     c                   except    saldo
033912991022     c                   end
033913991022     c                   move      mocmat        exmocmat
034000991022     c                   endsr
034001991022      *---------------------------------------------------------------------
034100991022     Osysprt    E            testa            01
034200991022     O                                           70 'Righe con saldi errati'
034400991022     O*
034401991022     O          E            testa       2
034402991022     o                                           14 'Codice'
034403991022     o                                           30 'Qta ordinata'
034404991022     o                                           50 'Qta assegnata'
034500991022     O          E            saldo       2
034501991022     o                       exmocmat            14
034600991022     o                       qtaor         2     30
034700991022     o                       qtaas         2     50
034701991022     o                       exmocdmn           110 '    /  /  '
034800991022     O          E            saldo       1
034801991022     o                       smpmat              14
035000991022     o                       smpqpr        2     30
035100991022     o                       smpqim        2     50
035200991022     o                       smpqip        2     66
035300991022     o                       smpqor        2     82
035400991022     o                       smpmag              96
035500991022     o                       smpdmn             110 '    /  /  '
