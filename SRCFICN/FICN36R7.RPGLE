000100060828     H* ACTGRP=*CALLER
000200060828     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300060828     ffifre02l  uf a e           k disk
000400060828     fndsla99l  if   e           k disk
000500060828
000600060828      *ingresso
000700060828     D param           DS
000800060828     D  data1                         8s 0
000900060828     D  parpo                         3s 0
001000060828
001100060828     d dataiso         s               d   datfmt(*iso)
001200060828     d dataeur         s               d   datfmt(*eur)
001300060828
001400060828     d Wdata8          DS
001500060828     d  dadata                 1      8  0
001600060828     d  adata                  9     16  0
001700060828     d  GioLav                17     21  0
001800060828      *
001900060828     d
002000060828      *--------------------------------------------------------------------
002100060828     c
002200060828      * verifico se esistono già le righe di rettifica per il mese in esame
002300060828     c     kfre          chain     fifre02l                           98
002400060828     c
002500060828      * se non esistono elaboro per la spalmatura dell'importo nei giorni
002600060828      * lavorativi
002700060828     c   98              exsr      elabora
002800060828
002900060828     c                   seton                                        lr
003000060828      *------------------------------------------------------
003100060828     c     elabora       begsr
003200060828      *------------------------------------------------------
003300060828     c                   clear                   import
003400060828     c                   clear                   totale
003500060828     c                   clear                   dataiso
003600060828      *
003700060828      * primo e ultimo giorno del mese
003800060828     c                   move      data1         dataiso
003900060828     c                   extrct    dataiso:*m    mese              2 0
004000060828     c                   z-add     mese          periodo
004100060828     c     dataiso       adddur    1:*m          dataiso
004200060828     c     dataiso       subdur    1:*d          dataiso
004300060828     c                   move      dataiso       data2             8 0
004400060828     c
004500060828      *reperisco il numero di giorni lavorativi nel mese
004600060828     c                   z-add     data1         dadata
004700060828     c                   move      data2         adata
004800060828     c                   CALL      'XSRLAV8'
004900060828     c                   PARM                    Wdata8
005000060828      *
005100060828      * reperisco le scritture contabili per spalmare l'importo
005200060828     c     kndsla        setll     ndsla99l
005300060828     c                   do        *hival
005400060828     c     kndsla        reade     ndsla99l                               99
005500060828     c   99              leave
005600060828      *
005700060828     c     slaimpdar     sub       slaimpave     import           13 3
005800060828     c                   add       import        totale           13 3
005900060828     c                   enddo
006000060828      * ricavo l'importo giornaliero dal totale
006100060905     c                   if        giolav > 0 and totale <> 0
006200060828     c     totale        div       giolav        impgior          13 3
006300060828     c                   exsr      scrivifre
006400060828     c                   endif
006500060828     c                   endsr
006600060828      *-------------------------------------------------
006700060828     c     scrivifre     begsr
006800060828      *-------------------------------------------------
006900060828     c                   clear                   fifre000
007000060828      *
007100060828     c                   do        *hival
007200060828      * se esco dal periodo ho finito di scrivere le rettifiche
007300060828     c                   if        dadata > data2
007400060828     c                   leave
007500060828     c                   endif
007600060828      *controllo i giorni lavorativi
007700060828     c                   z-add     dadata        adata
007800060828     c                   CALL      'XSRLAV8'
007900060828     c                   PARM                    Wdata8
008000060828
008100060828      * se risulta una giornata lavorativa scrivo la rettifica
008200060828     c                   if        giolav > 0
008300060831     c                   move      pdr           frepdr
008400060828     c                   z-add     dadata        freddc
008500060828     c                   z-add     impgior       fretim
008600060828     c                   z-add     1             frepno
008700060828     c                   move      'FAC'         frecre
008800060828     c                   move      'F'           fretsr
008900060828     c                   move      'C'           fretip
009000060828     c                   write     fifre000
009100060828     c                   endif
009200060828     c                   move      dadata        dataiso
009300060828     c     dataiso       adddur    1:*d          dataiso
009400060828     c                   move      dataiso       dadata
009500060828     c                   enddo
009600060828     c                   endsr
009700060828      *-------------------------------------------------
009800060828     c     *inzsr        begsr
009900060828      *-------------------------------------------------
010000060828     c     *entry        plist
010100060828     c                   parm                    param
010200060828
010300060828     c     kndsla        klist
010400060828     c                   kfld                    esercizio         5
010500060828     c                   kfld                    societa           3
010600060828     c                   kfld                    voce              8
010700060828     c                   kfld                    entita            8
010800060828     c                   kfld                    periodo           3 0
010900060828
011000060828     c                   movel     data1         esercizio
011100060828     c                   move      0             esercizio
011200060828     c                   move      '201'         societa
011300060828     c                   movel(p)  '305'         voce
011400060828     c                   movel(p)  parpo         entita
011500060828     c                   z-add     mese          periodo
011600060828
011700060828     c     kfre          klist
011800060828     c                   kfld                    tipo              1
011900060831     c                   kfld                    pdr               7 0
012000060828     c                   kfld                    dadata
012100060828     c                   kfld                    serv              1
012200060828     c                   kfld                    caus              3
012300060828
012400060828     c                   move      'C'           tipo
012500060828     c                   move      'F'           serv
012600060828     c                   move      'FAC'         caus
012700060831     c                   move      *zeros        pdr
012800060831     c                   movel     parpo         pdr
012900060828
013000060828      *cerco primo giorno lavorativo del mese
013100060828     c                   z-add     data1         dadata
013200060828     c                   do        *hival
013300060828     c                   z-add     dadata        adata
013400060828     c                   CALL      'XSRLAV8'
013500060828     c                   PARM                    Wdata8
013600060828     c                   if        giolav > 0
013700060828     c                   leave
013800060828     c                   else
013900060828     c                   move      dadata        dataiso
014000060828     c     dataiso       adddur    1:*d          dataiso
014100060828     c                   move      dataiso       dadata
014200060828     c                   endif
014300060828     c                   enddo
014400060828
014500060828     c                   endsr
014600060828      *-------------------------------------------------
