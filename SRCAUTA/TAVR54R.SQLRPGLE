000100091203     H DECEDIT('0,') DATEDIT(*YMD.) option(*nodebugio)
000200031105
000300031105      *****************************************************************
000400031105      *                                                               *
000500101221      *       AGGIORNA offerte FEDEX MERCI RPV 2,0                    *
000600031105      *                                                               *
000700031105      *****************************************************************
000800101221     FWfauo01l  uf   E           K DISK
000900031105
001100101221     FTIOFD01L  UF   E           K DISK
001101101221     FTIOPT01L  UF   E           K DISK
001102101221     FTIOGC01l  UF   E           K DISK
001800031105
002600031105      *------------------------------------------------------------------------*
002700031105
002800101221     D W_wfanrv        s                   like(wfanrv)
003000101216     D W_wfactrv       s                   like(wfactrv)
003001101216     D W_wfaprgv       s                   like(wfaprgv)
003100081128
004200101221     d AU_Cliente      s                   like(WFAnrv)
004300081201     d AU_Tariffa      s                   like(WFActrv)
004400081201     d AU_Progres      s                   like(WFAprgv)
004401101216
004402101216     d ela             s              1    inz(*off)
004800081201
004900031105     D W0140           S             14  0
005000031105     D Wdtgio          S              8  0
005100031105     D §oggi           S              8  0
005900031105
006000031105      *   D S   I N T E R N E / E S T E R N E
006100031105
006200031105     D WLBDAT          DS                  INZ
006300031105     D  G02DAT                 1      8  0
006400031105     D  G02INV                 9     16  0
006500031105     D  G02ERR                17     17
006600031105     D  G02TGI                18     22  0
006700031105
006800031105     D                 ds
007300031105
007500031105     D kpjba         e ds
007600031105
007700031105      *---------------------------------------------------------------------------------------------
007800081128      *------------------------------------------------------------------------*
007900081128      * Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008000081128     c
008100081128     c/EXEC SQL
008200081128     c+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008300081128     c/END-EXEC
008400081128     c*
008500081128     C/EXEC SQL
008600081128     C+ declare C1 cursor for
008700101221     C+ SELECT wfanrv, wfactrv, wfaprgv FROM EDPAUMENTI/WFauo00F
009000081128     c+ for read only
009100081128     C/END-EXEC
009200081128
009300081128     C/EXEC SQL
009400081128     C+ open C1
009500081128     C/END-EXEC
009600081128     C
009700081128     C/EXEC SQL
009800101221     C+ Fetch C1 into :W_wfanrv , :W_wfactrv , :W_wfaprgv
009900081128     C/END-EXEC
010000081128     C*
010100081128
010200081128     C*
010300081128     C                   dow       sqlcod = *zeros
010301101216     c                   eval      ela = *off
010400031105
010500031105      *
010600101221      * Aggancio TIOFD01l
010700101221     c     KTAD          setll     tiofd01l
010800031105     c                   do        *hival
010900101221     c     Ktad          reade     tiofd01l
011000031105      *
011100031105      * fine file
011200101221     c                   if        %eof(tiofd01l)
011300031105     c                   leave
011400031105     c                   endif
011500081201
011501101216     c                   eval      tadrpv = 2
011502101216     C                   eval      TADftr = *blanks
011503101216     C                   eval      TADdtr = §oggi
019100101216     C                   update    titad000
019101101216     c                   If        ela   = *off
019102101216     c                   eval      AU_cliente = tadksc
019103101216     c                   eval      AU_tariffa = tadctr
019104101216     c                   eval      AU_progres = tadprg
019105101216     c                   eval      ela   = *on
019106101216     c                   endif
024400051021
024500031105     C                   enddo
024501101220      *
024502101221      * Aggancio TIOPT01l
024503101221     c     KTAD          setll     tiopt01l
024504101220     c                   do        *hival
024505101221     c     Ktad          reade     tiopt01l
024506101220      *
024507101220      * fine file
024508101221     c                   if        %eof(tiopt01l)
024509101220     c                   leave
024510101220     c                   endif
024511101220      *
024512101220      * verifico se tipo tariffa prevede RPV (solo '0'= qle e '3'= kg) e rpv < 2
024514101220     c                   if        (tpttpg = '0' or tpttpg = '3') and
024515101220     c                              tptrpv < 2
024517101220     c                   eval      tptrpv = 2
024518101220     C                   eval      Tptduv = §oggi
024519101220     C                   eval      Tptdtr = §oggi
024520101220     C                   update    titpt000
024600101220     C                   endif
024601101220
024602101220     c                   enddo
024603101220      *
024604101221      * Aggancio TIOGC01l
024606101221     c     Ktad          chain     tiogc01l
024607101220      *
024608101220      * no fine file
024609101221     c                   if        %found(tiogc01l)
024612101220      *
024613101220      * modifico  RPV se < di 2
024615101220     c                   If        tgcrpv < 2
024616101220     c                   eval      tgcrpv = 2
024617101220     C                   eval      Tgcduv = §oggi
024618101220     C                   eval      Tgcdtr = §oggi
024619101220     C                   update    titgc000
024620101220     C                   endif
024621101220
024622101220     C                   endif
024623101220
024624101216      * aggiorno il nuovo RPV
024625101216     c                   If        ela   = *on
024626101221     c     kaut          chain     wfauo01l
024627101221    3c                   if        %found(wfauo01l)
024628101216     c                   eval      wfarpvn = tadrpv
024629101216     c                   eval      wfaela = 'S'
024630101216     c                   update    wfaut000
024631101216    3C                   endif
024632101216     c                   endif
024700081201     C/EXEC SQL
024800101221     C+ Fetch C1 into :W_wfanrv ,  :W_wfactrv , :W_wfaprgv
024900081201     C/END-EXEC
025000081201     C*
025100081201     C                   enddo
025200051021
025300081201     C*
025400081201     C/EXEC SQL
025500081201     C+ close C1
025600081201     C/END-EXEC
025700081201     C
025800081201
025900081201     c                   seton                                        lr
026000081201
062700031105      *****************************************************************
062800031105      * ROUTINE INIZIALE
062900031105      *****************************************************************
063000031105     C     *INZSR        BEGSR
063100031105
063200031105     C     *ENTRY        PLIST
063300031105     C                   PARM                    KPJBA
063400031105      *
063500031105     c
063600101216     C     KtaD          klist
063700101221     C                   kfld                    W_WFANRV
063800101216     C                   kfld                    W_WFACTRV
063900101216     C                   kfld                    W_WFAPRGV
064000031105
064100081201      *
064200081201     c
064300081201     C     Kaut          klist
064500081201     C                   kfld                    AU_Cliente
064600081201     C                   kfld                    AU_Tariffa
064700081201     C                   kfld                    AU_Progres
064800081201
065400081201      *
065500031105      * reperisce data del giorno
065600031105     C                   TIME                    W0140
065700031105      * UDATE IN GGMMAAAA
065800031105     C                   MOVE      W0140         WDTGIO
065900031105      * UDATE IN AAAAMMGG
066000031105     C                   Z-ADD     WDTGIO        G02DAT
066100031105     C                   MOVEL     *BLANK        G02ERR
066200031105     C                   CALL      'XSRDA8'
066300031105     C                   PARM                    WLBDAT
066400031105     C                   Z-ADD     G02INV        §oggi
066500031105
067400031105     C                   endsr
