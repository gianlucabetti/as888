000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300030613      *                   Programma di lancio FICN31R
000400000000      *****************************************************************
000500030613     ftntbe01l  if   e           k disk
000600030616     fAzcln01l  if   e           k disk
000700030617     FSYSPRT    O    F  132        PRINTER OFLIND(*INOA) usropn
000800030616      *
000900030616     d gio             S              1    DIM(31)
001000030616     d fes             S              1    DIM(31)
001100030616      *
001200030616     d dccg          e ds
001300030613      *
001400030616     d adataiso        s               d   datfmt(*iso)
001500030616     d dataisoP        s               d   datfmt(*iso)
001600030616     d dataisoA        s               d   datfmt(*iso)
001700030617     d dataggC         s               d   datfmt(*iso)
001800030617     d dataUDT         s               d   datfmt(*iso)
001900030617     d dataiso         s               d   datfmt(*iso)
002000030613     d dataiso2        s               d   datfmt(*iso)
002100030613     d dataiso3        s               d   datfmt(*iso)
002200030616     D*
002300030617     d data1           s               d   datfmt(*iso)
002400030617     d data2           s               d   datfmt(*iso)
002500030617     d data3           s               d   datfmt(*iso)
002600030617     D*
002700030616     d  DataAmg        s              8s 0
002800030617     d  Volte          s              3s 0
002900940926     D*
003000000000     D KPJBA         E DS
003100020307     D dsparam         DS
003200030613     d  dalladat                      8s 0
003300030613     d  alladat                       8s 0
003400030616     D*
003500030616     D AnMeGi          DS
003600030616     d  Kanno                         4s 0
003700030616     d  Kmese                         2s 0
003800030616     d  gg                            2s 0
003900940926     D*
004000000000     D WLBDAT          DS
004100940926     D  G02DAT                 1      8  0
004200940926     D  G02INV                 9     16  0
004300940926     D  G02ERR                17     17
004400940926     D  G02TGI                18     22  0
004500030613
004600030613     d Wdata8          DS
004700030613     d  dadata                 1      8  0
004800030613     d  adata                  9     16  0
004900030613     d  GioLav                17     21  0
005000030613      *
005100030617      *--------------------------------------------------------------*
005200030617      *  SERVE per testare il pgm (DATA8)
005300030617      *--------------------------------------------------------------*
005400030617     d*****  Data8          s              8    INZ('20030625')
005500030617     d  Data8          s              8    INZ('        ')
005600030613      *--------------------------------------------------------------*
005700030613      *
005800030613     c     ktbe          klist
005900030613     c                   kfld                    tbecod
006000030613     c                   kfld                    tbeke1
006100030613      *
006200030616     c     kcln          klist
006300030616     c                   kfld                    clntfp
006400030616     c                   kfld                    clntfa
006500030616     c                   kfld                    clnann
006600030616     c                   kfld                    clnmes
006700030616      *
006800000000     C     *ENTRY        PLIST
006900000000     C                   PARM                    KPJBA
007000030617      *
007100030617     c                   z-add     1             Volte
007200030617      *
007300030617      *  con la data8 impostata serve per testare il pgm
007400030617      *   inzializzarla vuota pre non testare
007500030617     c                   if        data8 <> *blank
007600030617     c                   movel     data8         wdat
007700030617     c                   open      sysprt
007800030617     c                   z-add     90            Volte
007900030617     c                   move      wdat          DataAMG
008000030617     c                   end
008100030617      *
008200030617     c                   DO        volte
008300030617      *
008400030617     c                   clear                   dsparam
008500030616      *
008600030616      * imposta e controlla Udate e se festivo esce dal programma
008700030616     c                   clear                   flg_fine          1
008800030616      *               *----------------------*
008900030616     c                   exsr      Ctl_Udate
009000030616      *               *----------------------*
009100030616     c                   if        Flg_Fine = 'S'
009200030616     c                   goto      Fine
009300030616     c                   end
009400030616      *
009500030616      * reperisce i gg.da calcolare indietro dalla CCG
009600030616      *               *----------------------*
009700030616     c                   exsr      TAB_CCG
009800030616      *               *----------------------*
009900030616      *
010000030616      * calcola il giorno C in cui sono stati fatti i conteggi autotrasportatori
010100030616      *               *----------------------*
010200030616     c                   exsr      Calc_ggC
010300030616      *               *----------------------*
010400030616      *------------------------------------------------------------
010500030616      * calcolo il A-Day partendo dal giorno (C) sempre -1
010600030616      *               *----------------------*
010700030616     c                   exsr      Calc_Aday
010800030616      *               *----------------------*
010900030616      *
011000030617      *               *----------------------*
011100030617     c                   exsr      Calc_ToDay
011200030617      *               *----------------------*
011300030616      *
011400030616      *------------------------------------------------------------
011500030617      * ho stabilito il A-DAY
011600030617      * ==========================              ---------
011700030617     c                   move      adataiso      alladat
011800030617      *                                         ---------
011900030616      * Se il D-day era un festivo infrasettimanale
012000030616      *  ho preso il primo giorno lavorativo valido
012100030616     c                   if        fesinfrset  ='S'
012200030616      *
012300030616      * il D-Day era un giorno festivo infrasettimanale
012400030616     c     dataiso2      adddur    1:*d          dataiso2
012500030616     c                   move      dataiso2      dalladat
012600030616      *
012700030616     c                   else
012800030617
012900030616      * controlla se prima del D-Day c'erano giorni festivi
013000030616      *               *----------------------*
013100030616     c                   exsr      ctrggfest
013200030616      *               *----------------------*
013300030616     c                   end
013400030616      *
013500030616      * Controllo se Il giorno precedente all'udate era festivo
013600030616      *  e se lo era occorre allargare il range "dalla data" lasciando
013700030616      *   fisso "alla data"
013800030616      *               *----------------------*
013900030616     c                   exsr      gg_prec_udate
014000030616      *               *----------------------*
014100030613
014200030617     c************       goto      fine
014300000000      *
014400030613      *--------------------------------------------------------------*
014500000000      *             M A I N      L I N E
014600000000      *--------------------------------------------------------------*
014700000000      *
014800020307     c                   movel     dsparam       kpjbu
014900020626     c                   move      'FN31'        kcoaz
015000031016     c                   move      *blank        kbuff
015100030613      *
015200020624      *richiamo sql pulizia saldi e aggiornamento flag FCE di record saldato
015300110324     c*********          call      'BCH10'
015400110324     c*********          parm                    kpjba
015500000000      *
015600110324      *  in sequenza
015700110324     c                   call      'FICN31R'
015800110324     c                   parm                    kpjba
015900110324      *
016000030613     c     fine          tag
016100030617      *
016200030617      * per provare il pgm in stampa
016300030617     c                   if        volte > 1
016400030617     c                   move      dataUDT       data1
016500030617     c                   if        alladat >0
016600030617     c                   move      dalladat      data2
016700030617     c                   move      alladat       data3
016800030617     c                   except    giorni
016900030617     c                   end
017000030617     c     data1         adddur    1:*d          data1
017100030617     c                   move      data1         DataAMG
017200030617     c                   end
017300030617     c                   ENDDO
017400030617      *
017500000000     C                   SETON                                        LR
017600000000      *--------------------------------------------------------------*
017700030616      * IMposta e controlla UDATE
017800030616      *--------------------------------------------------------------*
017900030616     c     ctl_Udate     begsr
018000030616
018100030617     c                   if        wdat = 0
018200030616     C                   TIME                    W0120            14 0
018300030616     C                   MOVEl     W0120         Wora              6 0
018400030616     C                   MOVE      W0120         WDAT              8 0
018500030616      *
018600030616     C                   Z-ADD     WDAT          G02DAT
018700030616     C                   MOVEL     *BLANK        G02ERR
018800030616     C                   CALL      'XSRDA8'
018900030616     C                   PARM                    WLBDAT
019000030617     C                   move      G02INV        dataAMG
019100030617     c                   end
019200030617      *
019300030616     C* UDATE A 8 IN AAAA/MM/GG
019400030617     C                   move      DataAMG       dataUDT
019500030617     C                   movel     DataAMG       AnMeGi
019600030616      *
019700030616      *controllo se udate è festivo non lancio l'elaborazione
019800030616      * ed esce subito
019900030616     c                   clear                   clntfa
020000030616     c                   clear                   clntfp
020100030616     c                   z-add     kanno         clnann
020200030616     c                   z-add     kmese         clnmes
020300030616     c     kcln          chain     azcln01l
020400030616     c                   if        %found(azcln01l)
020500030616     c                   movea     clnmat        gio
020600030616     c                   movea     clnpom        fes
020700030616     c                   end
020800030616      * esce subito se festivo o
020900030616      *  se sabato o domenica
021000030616     c                   clear                   flg_fine
021100030616     c                   if        gio(gg) = '6' or gio(gg) = '7' or
021200030616     c                             fes(gg) = 'F'
021300030616     c                   eval      flg_fine = 'S'
021400030616     c                   end
021500030616      * giorno precedente
021600030617     c     dataUDT       subdur    1:*d          dataisoP
021700030616      *
021800030616     c                   Endsr
021900030616      *--------------------------------------------------------------*
022000030616      * aggancia tab.CCG
022100030616      *--------------------------------------------------------------*
022200030616     c     TAB_CCG       begsr
022300030616
022400030616      *reperisco limiti da tabella per calcolare il D-Day
022500030616     c                   movel(p)  '1'           tbeke1
022600030616     c                   movel     'CCG'         tbecod
022700030616     c     ktbe          chain     tntbe01l                           90
022800030616     c  n90              movel     tbeuni        dccg
022900030616      *
023000030616     c                   Endsr
023100030616      *--------------------------------------------------------------*
023200030616      *  Calcola il giorno (C) tenendo conto del sabato e domenica
023300030616      *--------------------------------------------------------------*
023400030616     c     Calc_ggC      begsr
023500030616      *
023600030616      * va tutto rapportato indietro al giorno C in cui vengono fatte le
023700030616      * valorizzazioni  (2 gg.prima)
023800030617     c     dataUDT       subdur    2:*d          dataggC
023900030616      *
024000030616      * se è un lunedì/martedì sottraggo altri 2 giorni (sabato/domenica)
024100030616     c                   if        gio(gg) = '1' or gio(gg) = '2'
024200030617     c     dataggC       subdur    2:*d          dataggC
024300030616     c                   end
024400030616      *
024500030616     c                   endsr
024600030616      *--------------------------------------------------------------*
024700030616      *  calcola il D-Day Lavorativo
024800030616      *--------------------------------------------------------------*
024900030616     c     Calc_Aday     begsr
025000030617      *
025100030616      * calcolo il A-Day partendo dal giorno (C) meno 1
025200030616      *  tenendo conto dei giorni lavorativi.
025300030616      *
025400030617     c     dataggC       subdur    1:*d          dataiso2
025500030616      *
025600030616     c                   move      dataiso2      adataiso
025700030616     c                   move      dataiso2      DataAmg
025800030616      *------------------------------------------------------------
025900030616      * Controlla se
026000030616      *  è un festivo infrasettimanale e non è sabato/domenica
026100030616      *               *----------------------*
026200030616     c                   exsr      Calc_festivo
026300030616      *               *----------------------*
026400030616      * -----------
026500030616      * Cerca definitivamente il D-day lavorativo
026600030616      * -----------
026700030616     c                   do        *hival
026800030616     c                   move      dataiso2      dadata
026900030616     c                   move      dataiso2      adata
027000030616     c                   CALL      'XSRLAV8'
027100030616     c                   PARM                    Wdata8
027200030616      * se corrisponde ho trovato il D-Day
027300030616     c                   if        giolav <> 0
027400030616     c                   leave
027500030616     c                   else
027600030616      * altrimenti continuo a sottrarre i giorni finchè non trovo
027700030616      *  il primo girno lavorativo valido.
027800030616     c     dataiso2      subdur    1:*d          dataiso2
027900030616     c                   iter
028000030616     c                   endif
028100030616     c                   enddo
028200030616      *
028300030617      *  Se era un weekend  sabato o domenica imposta il primo giorno
028400030617      *   lavorativo valido
028500030617     c                   if        fesweekend = 'S'
028600030617     c                   move      dataiso2      adataiso
028700030617     c                   endif
028800030617      *
028900030616     c                   endsr
029000030616      *--------------------------------------------------------------*
029100030617      *  calcola gli N giorni lavorativi indietro dal A-Day
029200030616      *--------------------------------------------------------------*
029300030617     c     Calc_ToDay    begsr
029400030616      *
029500030617     c                   move      adataiso      dataiso2
029600030617      *
029700030617     c                   do        *hival
029800030617     c                   move      dataiso2      dadata
029900030617     c                   move      adataiso      adata
030000030617     c                   CALL      'XSRLAV8'
030100030617     c                   PARM                    Wdata8
030200030617      * se corrisponde ho trovato il D-Day
030300030617     c                   if        giolav = §cgafce
030400030617     c                   leave
030500030617     c                   else
030600030617      * altrimenti continuo a sottrarre i giorni finchè non trovo
030700030617      *  il primo girno lavorativo valido.
030800030617     c     dataiso2      subdur    1:*d          dataiso2
030900030617     c                   iter
031000030617     c                   endif
031100030617      *
031200030617     c                   enddo
031300030617      *
031400030617      *
031500030617     c                   endsr
031600030616      *--------------------------------------------------------------*
031700030616      *  se è un festivo infrasettimanale
031800030616      *--------------------------------------------------------------*
031900030616     c     Calc_festivo  begsr
032000030616      *
032100030616      * Festivo infrasettimanale
032200030616     c                   clear                   fesinfrset        1
032300030616     c                   clear                   fesweekend        1
032400030616      *
032500030616     C                   movel     DataAmg       AnMeGi
032600030616     c                   clear                   clntfa
032700030616     c                   clear                   clntfp
032800030616     c                   z-add     kanno         clnann
032900030616     c                   z-add     kmese         clnmes
033000030616     c     kcln          chain     azcln01l
033100030616     c                   if        %found(azcln01l)
033200030616     c                   movea     clnmat        gio
033300030616     c                   movea     clnpom        fes
033400030616     c                   end
033500030616     c                   if        fes(gg) = 'F'
033600030616     c                   if        gio(gg) <> '6' and gio(gg) <> '7'
033700030616     c                   move      'S'           fesinfrset
033800030616     c                   else
033900030616     c                   move      'S'           fesweekend
034000030616     c                   end
034100030616     c                   end
034200030616      *
034300030616     c                   endsr
034400030616      *--------------------------------------------------------------*
034500030616      * Prima del D-day c'erano giorni festivi (sabati e domeniche)
034600030616      *--------------------------------------------------------------*
034700030616     c     ctrggfest     begsr
034800030616
034900030617     c                   move      dataiso2      dalladat
035000030616      *
035100030616      * verifico se i giorni precedenti al D-Day sono festivi per accorparli
035200030616     c     dataiso2      subdur    1:*d          dataiso3
035300030616     c                   do        *hival
035400030616     c                   move      dataiso3      dadata
035500030616     c                   move      dataiso2      adata
035600030616     c                   CALL      'XSRLAV8'
035700030616     c                   PARM                    Wdata8
035800030616      *
035900030616      * quando i giorni lavorativi diventano 2 vuol dire che ho chiuso il
036000030616      * sottoperiodo di raggruppamento del D-DAY
036100030616      * esempio: se il D-DAY è lunedi il sabato e domenica precedenti
036200030616      * vanno sommati assieme al D-Day
036300030616
036400030616      * se trovo il secondo giorno lavorativo aggiungo uno per tornare al
036500030616      * giorno seguente (calendario) che è la data di partenza del periodo
036600030616      * del D-day
036700030616     c                   if        giolav = 2
036800030616     c     dataiso3      adddur    1:*d          dataiso3
036900030616     c                   leave
037000030616     c                   else
037100030616      * altrimenti continuo a sottrarre i giorni da udate
037200030616     c     dataiso3      subdur    1:*d          dataiso3
037300030616     c                   iter
037400030616     c                   endif
037500030616     c                   enddo
037600030616
037700030616      * ho stabilito eventuale ragguppamento del D-DAY
037800030616     c                   move      dalladat      dataiso
037900030616
038000030616      *se ci sono delle festività allora allarga il range comprendendole
038100030616     c                   if        dataiso <> dataiso3
038200030616     c                   move      dataiso3      dalladat
038300030616     c                   end
038400030616
038500030616     c                   endsr
038600030616      *--------------------------------------------------------------*
038700030616      * Prima dell'udate era un giorno festivo ?
038800030616      *--------------------------------------------------------------*
038900030616     c     gg_prec_udate begsr
039000030616      *
039100030616     c                   z-add     0             subgg             3 0
039200030616      * Controlla
039300030616      *   se era un Festivo infrasettimanale
039400030616     c                   move      dataisoP      DataAmg
039500030616     C                   movel     DataAmg       AnMeGi
039600030616     c                   clear                   clntfa
039700030616     c                   clear                   clntfp
039800030616     c                   z-add     kanno         clnann
039900030616     c                   z-add     kmese         clnmes
040000030616     c     kcln          chain     azcln01l
040100030616     c                   if        %found(azcln01l)
040200030616     c                   movea     clnmat        gio
040300030616     c                   movea     clnpom        fes
040400030616     c                   end
040500030616      *
040600030616     c                   if        fes(gg) = 'F'
040700030616      *
040800030616      * finchè c'erano giorni festivi prima del D-Day
040900030616      *  e conta di quanti giorni deve andare indietro
041000030616     c                   dow       fes(gg) = 'F'
041100030616      *
041200030616      * conta i gg. senza i sabati e domeniche
041300030616     c                   if        gio(gg) <> '6' and gio(gg) <> '7'
041400030616     c                   add       1             subgg             3 0
041500030616     c                   end
041600030616      *
041700030616      * va ancora indietro di 1 e controlla nuovamente
041800030616     c     dataisoP      subdur    1:*d          dataisoP
041900030616     c                   move      dataisoP      DataAmg
042000030616     C                   movel     DataAmg       AnMeGi
042100030616     c                   clear                   clntfa
042200030616     c                   clear                   clntfp
042300030616     c                   z-add     kanno         clnann
042400030616     c                   z-add     kmese         clnmes
042500030616     c     kcln          chain     azcln01l
042600030616     c                   if        %found(azcln01l)
042700030616     c                   movea     clnmat        gio
042800030616     c                   movea     clnpom        fes
042900030616     c                   end
043000030616      * se era festivo cerca se c'era un'altro giorno festivo prima
043100030616     c                   enddo
043200030616      *
043300030616     c                   move      dalladat      dataiso2
043400030616     c     dataiso2      subdur    subgg:*d      dataiso2
043500030616     c                   move      dataiso2      dalladat
043600030616      *
043700030617     c                   if        subgg > 0
043800030617     c                   exsr      ctrggfest
043900030616     c                   end
044000030616      *
044100030617     c                   end
044200030617      *
044300030616     c                   endsr
044400030616      *--------------------------------------------------------------*
044500030617     OSYSPRT    E            giorni      1
044600030617     o                                            5 ' '
044700030617     o                       data1             +  1
044800030617     o                       data2             +  5
044900030617     o                       data3             +  2
