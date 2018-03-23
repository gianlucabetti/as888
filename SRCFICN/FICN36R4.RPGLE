000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300030415     ffifre02l  uf a e           k disk
000400030414     fndsla99l  if   e           k disk
000500030110
000600030414      *ingresso
000700030414     D param           DS
000800030414     D  data1                         8s 0
000900030414     D  parpo                         3s 0
001000060831
001100030108     d dataiso         s               d   datfmt(*iso)
001200030108     d dataeur         s               d   datfmt(*eur)
001300030414
001400030414     d Wdata8          DS
001500030414     d  dadata                 1      8  0
001600030414     d  adata                  9     16  0
001700030414     d  GioLav                17     21  0
001800030414      *
001900030109     d
002000000607      *--------------------------------------------------------------------
002100000607     c
002200030415      * verifico se esistono già le righe di rettifica per il mese in esame
002300030415     c     kfre          chain     fifre02l                           98
002400030415     c
002500030415      * se non esistono elaboro per la spalmatura dell'importo nei giorni
002600030415      * lavorativi
002700030415     c   98              exsr      elabora
002800020517
002900020521     c                   seton                                        lr
003000020521      *------------------------------------------------------
003100020521     c     elabora       begsr
003200020521      *------------------------------------------------------
003300030414     c                   clear                   import
003400030414     c                   clear                   totale
003500030415     c                   clear                   dataiso
003600030414      *
003700030414      * primo e ultimo giorno del mese
003800030414     c                   move      data1         dataiso
003900030414     c                   extrct    dataiso:*m    mese              2 0
004000030415     c                   z-add     mese          periodo
004100030414     c     dataiso       adddur    1:*m          dataiso
004200030414     c     dataiso       subdur    1:*d          dataiso
004300030414     c                   move      dataiso       data2             8 0
004400030414     c
004500030415      *reperisco il numero di giorni lavorativi nel mese
004600030414     c                   z-add     data1         dadata
004700030414     c                   move      data2         adata
004800030414     c                   CALL      'XSRLAV8'
004900030414     c                   PARM                    Wdata8
005000030305      *
005100030415      * reperisco le scritture contabili per spalmare l'importo
005200030414     c     kndsla        setll     ndsla99l
005300030414     c                   do        *hival
005400030414     c     kndsla        reade     ndsla99l                               99
005500030414     c   99              leave
005600030415      *
005700030415     c     slaimpdar     sub       slaimpave     import           13 3
005800030414     c                   add       import        totale           13 3
005900030414     c                   enddo
006000030415      * ricavo l'importo giornaliero dal totale
006100030422     c                   if        giolav > 0 and totale > 0
006200030414     c     totale        div       giolav        impgior          13 3
006300030414     c                   exsr      scrivifre
006400030414     c                   endif
006500030414     c                   endsr
006600030120      *-------------------------------------------------
006700030414     c     scrivifre     begsr
006800030120      *-------------------------------------------------
006900030414     c                   clear                   fifre000
007000030414      *
007100030414     c                   do        *hival
007200030414      * se esco dal periodo ho finito di scrivere le rettifiche
007300030414     c                   if        dadata > data2
007400030414     c                   leave
007500030414     c                   endif
007600030415      *controllo i giorni lavorativi
007700030414     c                   z-add     dadata        adata
007800030414     c                   CALL      'XSRLAV8'
007900030414     c                   PARM                    Wdata8
008000030414
008100030415      * se risulta una giornata lavorativa scrivo la rettifica
008200030414     c                   if        giolav > 0
008300060831     c                   move      pdr           frepdr
008400030414     c                   z-add     dadata        freddc
008500030414     c                   z-add     impgior       fretim
008600030417     c                   z-add     1             frepno
008700030414     c                   move      'FAF'         frecre
008800030414     c                   move      'F'           fretsr
008900030414     c                   move      'C'           fretip
009000030414     c                   write     fifre000
009100030123     c                   endif
009200030414     c                   move      dadata        dataiso
009300030414     c     dataiso       adddur    1:*d          dataiso
009400030414     c                   move      dataiso       dadata
009500030414     c                   enddo
009600030120     c                   endsr
009700020521      *-------------------------------------------------
009800020521     c     *inzsr        begsr
009900020521      *-------------------------------------------------
010000020626     c     *entry        plist
010100030414     c                   parm                    param
010200030414
010300030109
010400030414     c     kndsla        klist
010500030414     c                   kfld                    esercizio         5
010600030414     c                   kfld                    societa           3
010700030414     c                   kfld                    voce              8
010800030414     c                   kfld                    entita            8
010900030414     c                   kfld                    periodo           3 0
011000030305
011100030414     c                   movel     data1         esercizio
011200030414     c                   move      0             esercizio
011300030414     c                   move      '201'         societa
011400030414     c                   movel(p)  '401'         voce
011500030414     c                   movel(p)  parpo         entita
011600030414     c                   z-add     mese          periodo
011700030415
011800030415     c     kfre          klist
011900030415     c                   kfld                    tipo              1
012000060831     c                   kfld                    pdr               7 0
012100030415     c                   kfld                    dadata
012200030415     c                   kfld                    serv              1
012300060831     c                   kfld                    caus              3
012400030415
012500030415     c                   move      'C'           tipo
012600030415     c                   move      'F'           serv
012700060831     c                   move      'FAF'         caus
012800060831     c                   move      *zeros        pdr
012900060831     c                   movel     parpo         pdr
013000030415
013100030415      *cerco primo giorno lavorativo del mese
013200030415     c                   z-add     data1         dadata
013300030415     c                   do        *hival
013400030415     c                   z-add     dadata        adata
013500030415     c                   CALL      'XSRLAV8'
013600030415     c                   PARM                    Wdata8
013700030415     c                   if        giolav > 0
013800030415     c                   leave
013900030415     c                   else
014000030415     c                   move      dadata        dataiso
014100030415     c     dataiso       adddur    1:*d          dataiso
014200030415     c                   move      dataiso       dadata
014300030415     c                   endif
014400030415     c                   enddo
014500030415
014600020521     c                   endsr
014700030414      *-------------------------------------------------
