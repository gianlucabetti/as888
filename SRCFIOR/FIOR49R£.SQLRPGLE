000100020808      *PARMS CLOSQLCSR(*ENDMOD)
000200020808      *PARMS DYNUSRPRF(*OWNER)
000300010604     H DECEDIT('0,') DATEDIT(*DMY.)
000400080910      *------------------------------------------------------------------*
000500080908      *
  Statistica di controllo procedura assegnazione automatica ORM   ?
000600080910      *------------------------------------------------------------------*
000700080910     ffnorg01l  if   e           k disk
000800010605     fazorg01l  if   e           k disk
000900010611     ftntbe01l  if   e           k disk
001000010620     ftabel00f  if   e           k disk
001100080924     fwfroa01l  uf a e           k disk    usropn
001200080910     ffior49p   o    e             printer oflind(*in90)
001300080910     ffior49p1  o    e             printer oflind(*in92) usropn                 controllo
001400080910     ffior49p2  o    e             printer oflind(*in91) usropn                 comodo EDP
001500010614
001600010614      *----------------------------------------------------*
001700010614      *  RIEPILOGO INDICATORI
001800010614      *----------------------------------------------------*
001900080912      * 10 - totali generali
002000010620      * 12 - sede
002100080910      * 13 - stampo per p.o. e dettaglio per data
002200080923      * 15 - stampa analitica
002300020808      * 30 - Lookup
002400080910      * 90 - overflow file stampa statistica
002500080910      * 91 - overflow file stampa dettaglio   A1
002600080910      * 92 - overflow file stampa dettraglio  B1
002700010605      *----------------------------------------------------*
002800010608     d wgiorno         s              2  0
002900010604     d w0140           s             14  0
003000010604     d wdtgio          s              8  0
003100010604     d wora            s              6  0
003200010604     d wdate           s              8  0
003300010606     d dataela         s              8  0
003400010621
003500010621     d lenght          s             15  5 inz(80)                              lunghezza cmd CLP
003600010621     d comman          s             80                                         valore cmd CLP
003700030912     d wlib            s             10
003800080911     d wdet            s              1
003900080911     d wabi            s              2
004000080917     d wdao            s              8  0
004100080911     d wpor            s                   like(ormpor)
004200080923     d primariga       s              1
004300080923     d stafl3          s                   like(orgfl3)
004400010605
004500080910     d savdao          s                   like(ormdao) inz
004600080910     d savpor          s                   like(ormpor) inz
004700080924     d
004800080924     d kroapoe         s                   like(roapoe)
004900080924     d kroadta         s                   like(roadae)
005000010607
005100010612     d xd              s              5i 0 inz(*zeros)                          indice sk date
005200010612     d xa              s              5i 0 inz(*zeros)                          indice sk po da ela.
005300020807     d Xb              s              5i 0 inz(*zeros)                          indice sk po TRUL26
005400031002     d xc              s              5i 0 inz(*zeros)                          indice sk OSR
005500080912     d genA            s              7  0 inz(*zeros)                          indice sk OSR
005600080912     d genA1           s              7  0 inz(*zeros)                          indice sk OSR
005700080912     d genB            s              7  0 inz(*zeros)                          indice sk OSR
005800080912     d genB1           s              7  0 inz(*zeros)                          indice sk OSR
005900080912     d genC            s              7  0 inz(*zeros)                          indice sk OSR
006000080912     d genC1           s              7  0 inz(*zeros)                          indice sk OSR
006100080923     d finA            s              7  0 inz(*zeros)                          indice sk OSR
006200080923     d finA1           s              7  0 inz(*zeros)                          indice sk OSR
006300080923     d finB            s              7  0 inz(*zeros)                          indice sk OSR
006400080923     d finB1           s              7  0 inz(*zeros)                          indice sk OSR
006500080923     d finC            s              7  0 inz(*zeros)                          indice sk OSR
006600080923     d finC1           s              7  0 inz(*zeros)                          indice sk OSR
006700020808
006800080912     d StringaSql      s           4096    Varying
006900010605
007000010608     d wdtda           s               d   datfmt(*iso)
007100010608     d wdtal           s               d   datfmt(*iso)
007200031002
007300010605
007400010605      *   S C H I E R E
007500010605
007600010608     d skdate          s              8  0 dim(31)                              date da elab.
007700020807     d skpoel          s              3  0 dim(250)                             po da elab.
007800080911     D $pog            s              3    dim(250)
007900080911     D $DIG            s              1    dim(20)
008000080911     D $ARG            s              3    dim(50)
008100080911     d
008200030912     d kfile           s             12    dim(18000)                           file (key)
008300010605     d                                     varying
008400010606     d                                     inz
008500030912     d kfile1          s             11    dim(18000)                           file (key) appoggio
008600030912     d                                     varying
008700030912     d                                     inz
008800030912     d rfile           s            456    dim(18000)                           file (record)
008900010605     d                                     varying
009000010606     d                                     inz
009100031002
009200031002     d skosr           s              5    dim(999)                             serie da non elab.
009300010604
009400010604      *   D S   I N T E R N E / E S T E R N E
009500010605
009600010604     d wlbdat          ds                  inz
009700010604     d  g02dat                 1      8  0
009800010604     d  g02inv                 9     16  0
009900010604     d  g02err                17     17
010000010604     d  g02tgi                18     22  0
010100031002
010200031002     d wosr            ds
010300031002     d  wOrmPoe                1      3
010400031002     d  wOrmNsr                4      5
010500010604
010600080908     d fior48ds        ds                  inz
010700080908     d  I48poel                1      3  0
010800080908     d  I48dtda                4     11  0
010900080908     d  I48dtal               12     19  0
011000080908     d  I48deta1              20     20
011100080908     d  I48detb1              21     21
011200080908     d  I48dtpo               22     29  0
011300080908     d  I48Sede               30     30
011400080923     d  I48tip                31     31
011500080923     d  I48ela                32     32
011600080908
011700080908     D TIBS02DS      E DS
011800080908     D TRUL31DS      E DS
011900080908     D TRUL31DS2     E DS
012000080908     D TIBS34DS      E DS
012100080908     d Azuteds       e ds                  extname(Azute00f)
012200080908     d dDatiute      e ds
012300080908     d dLat          e ds
012400080908     d dute01        e ds
012500080917     d og147         e ds
012600080908
012700031002     d dosr          e ds
012800020807
012900020807     d FnormxxDs     e ds                  ExtName(Fnorm00f)
013000020807
013100010604     d kpjba         e ds
013200010604      *--------------------------------------------------------------*
013300010604
013400080910      * Stampo le selezioni
013500010604     c                   exsr      PRESELEZ
013600080910      *
013700080909      * Se richiesta una data o una filiale, faccio un solo SQL
013800080910      *  altrimenti per ogni data elaboro tutti i p.o.
013900080909     c                   if        skdate(2)=0 or skpoel(2)=0
014000080910     c                   exsr      UNiSQL
014100080911     c
014200080911     c                   exsr      Conteggio
014300080911     c
014400080911     c* ultima riga di stampa e fine stampa
014500080924     c                   if        totA>0
014600080911     c                   exsr      Stampacont
014700080911     c                   endif
014800080911
014900080912     c* totale generale
015000080924     c                   if        i48ela<>'F'
015100080923     c                   seton                                        10
015200080912     c                   EXSR      TotaleGEN
015300080911      * fine stampa
015400080911     c                   write     or49e1
015500080924     c                   endif
015600080911     c
015700080909     c                   else
015800080911     c* loop sulle filiale per stamparne una alla volta per tutte le date
015900080911     c                   z-add     1             xa
016000080911     c                   dow       skpoel(xa)>0
016100080911     c                   z-add     skpoel(Xa)    wpor
016200080911     c
016300080911     c                   movel     wpor          stapoel
016400080911     c     wpor          chain     azorg01l
016500080911     c                   if        %found(azorg01l)
016600080911     c                   movel     orgdes        despoel
016700080923     c                   movel     orgfl3        stafl3
016800080911     c                   exsr      DECODI_ARE
016900080911     c                   else
017000080911     c                   eval      despoel=*all'?'
017100080911     c                   endif
017200080923     c                   clear                   primariga
017300080911     c
017400080911     c
017500080911     c                   exsr      filSQL
017600080911     c
017700080911     c                   exsr      Conteggio
017800080911     c
017900080911     c* ultima riga di stampa e fine stampa
018000080924     c                   if        totA>0
018100080911     c                   exsr      Stampacont
018200080911     c                   endif
018300080924     c
018400080923     c                   if        totA>0 and *in15
018500080924     c                   if        i48ela<>'F'
018600080923     c* Accenfo indicatore 16 per non stampare le righe di sepaerazione
018700080923     c                   seton                                        16
018800080923     c                   EXSR      TotaleGEN
018900080923     c                   setoff                                       16
019000080924     c                   endif
019100080924     c
019200080923     c* mi salvo i totali generali della filiale per fare il totalone
019300080923     c*  alla fine
019400080923     c                   add       genA          finA
019500080923     c                   add       genA1         finA1
019600080923     c                   add       genB          finB
019700080923     c                   add       genB1         finB1
019800080923     c                   add       genC          finC
019900080923     c                   add       genC1         finC1
020000080923     c
020100080923     c                   clear                   genA
020200080923     c                   clear                   genA1
020300080923     c                   clear                   genB
020400080923     c                   clear                   genB1
020500080923     c                   clear                   genC
020600080923     c                   clear                   genC1
020700080923     c                   endif
020800080911     c
020900080911     c                   eval      *in91=*on
021000080911     c                   eval      *in92=*on
021100080923     c
021200080911     c                   clear                   savpor
021300080911     c                   clear                   savdao
021400080911     c
021500080911     c                   clear                   totA
021600080911     c                   clear                   totA1
021700080911     c                   clear                   totB
021800080911     c                   clear                   totB1
021900080911     c                   clear                   totC
022000080911     c                   clear                   totC1
022100080911
022200080911     c                   add       1             xa
022300080911     c                   enddo
022400080911     c
022500080912     c* totale generale
022600080923     c* Se stampa sintetica nuovo il totale finale
022700080924     c                   if        i48ela<>'F'
022800080923     c                   if        *in15
022900080923     c                   z-add     finA          genA
023000080923     c                   z-add     finA1         genA1
023100080923     c                   z-add     finB          genB
023200080923     c                   z-add     finB1         genB1
023300080923     c                   z-add     finC          genC
023400080923     c                   z-add     finC1         genC1
023500080923     c*
023600080923     c                   setoff                                       15
023700080923     c                   endif
023800080923     c
023900080923     c                   seton                                        10
024000080912     c                   EXSR      TotaleGEN
024100080911      * fine stampa
024200080911     c                   write     or49e1
024300080911     c*
024400080909     c                   endif
024500080924     c                   endif
024600010605
024700010608      * fine programma
024800010605     c                   eval      *inlr = *on
024900010605
025000010604      *--------------------------------------------------------------*
025100010604      * PREPARA LA STAMPA DELLA VIDEATA DELLE SELEZIONI
025200010604      *--------------------------------------------------------------*
025300010604     c     PRESELEZ      begsr
025400010618
025500010618      * stampo la prima pagina con le selezioni
025600080911     c                   eval      *in12 = (i48sede = 'S')
025700010618
025800080910     c                   if        i48poel = 0
025900080909     c                   eval      despo = 'Tutte le fil.abilitate'
026000010618     c                   else
026100080911     c     i48poel       chain     azorg01l
026200010618     c                   if        %found(azorg01l)
026300010618     c                   movel     orgdes        despo
026400080910     c                   movel     orgdes        despoel
026500080911     c                   movel     i48poel       stapoel
026600080923     c                   movel     orgfl3        stafl3
026700080910     c
026800080911     c* DEcodifica area
026900080911     c                   exsr      decodi_ARE
027000010618     c                   endif
027100010618     c                   endif
027200080923     c
027300080923     c* decodifica tipo stampa
027400080923     c                   if        i48tip='S'
027500080923     c                   eval      statipo='SINTETICA'
027600080923     c                   seton                                        15
027700080923     c* se una data solo allora stampo come se fosse analitica
027800080923     c*  perchè non c'e' differenza  con la sintetica
027900080923     c                   if        i48tip='S' AND  (i48dtal=0
028000080923     c                             or i48dtda=i48dtal)
028100080923     c                   eval      *in15 = *off
028200080923     c                   endif
028300080923     c
028400080923     c                   else
028500080923     c                   eval      statipo='ANALITICA'
028600080923     c                   setoff                                       15
028700080923     c                   endif
028800080924     c* decodifica tipo elaborazione
028900080924     c                   select
029000080924     c                   WHEN      i48ela='S'
029100080924     c                   eval      staela='SOLO STAMPA'
029200080924
029300080924     c                   WHEN      i48ela='F'
029400080924     c                   eval      staela='SOLO FILE  '
029500080924     C
029600080924     c                   OTHER
029700080924     c                   eval      staela='STAMPA E CREAZIONE FILE'
029800080924     C                   ENDSL
029900010618
030000010618     c                   clear                   wlbdat
030100080911     c                   move      i48dtda       g02inv
030200010618     c                   movel     '3'           G02err
030300010618     c                   call      'XSRDA8'
030400010618     c                   parm                    wlbdat
030500010618     c                   movel     g02dat        datada
030600010618
030700010618     c                   clear                   wlbdat
030800080911     c                   move      i48dtal       g02inv
030900010618     c                   movel     '3'           G02err
031000010618     c                   call      'XSRDA8'
031100010618     c                   parm                    wlbdat
031200010618     c                   movel     g02dat        dataal
031300080911     c                   write     or49s1
031400010618
031500080911     c                   if        i48deta1='S'
031600080911     c                   write     or49s1c
031700080911     c                   endif
031800080911     c
031900080911     c                   if        i48detb1='S'
032000080911     c                   write     or49s1b
032100080911     c                   endif
032200080911     c
032300080924     c* Se devo creare il file prima pulisco se già ci sono i record
032400080924     c                   if        i48ela <>'S'
032500080924     c                   open      wfroa01l
032600080924
032700080924     c                   eval      xa = 1
032800080924     c                   do        250           xa
032900080924     c                   if        skpoel(xa) = *zeros
033000080924     c                   leave
033100080924     c                   endif
033200080924
033300080924     c     *iso          movel     i48dtda       wdtda
033400080924     c                   if        i48dtal>0
033500080924     c     *iso          movel     i48dtal       wdtal
033600080924     c                   else
033700080924     c     *iso          movel     i48dtda       wdtal
033800080924     c                   endif
033900080924
034000080924     c                   dow       wdtda<=wdtal
034100080924     c                   eval      kroapoe = skpoel(xa)
034200080924     c                   move      wdtda         kroadta
034300080924     c     kroa          setll     wfroa01l
034400080924     c                   if        %equal(wfroa01l)
034500080924     c     kroa          reade     wfroa01l
034600080924     c                   dow       not%eof(wfroa01l)
034700080924     c                   delete    wfroa000
034800080924     c     kroa          reade     wfroa01l
034900080924     c                   enddo
035000080924     c                   endif
035100080924     c
035200080924     c                   adddur    1:*d          wdtda
035300080924     c                   enddo
035400080924
035500080924     c                   enddo
035600080924     c                   endif
035700080924
035800010604     c                   endsr
035900080910      *--------------------------------------------------------------*
036000080910      * SQL  - SQL unico
036100080910      *--------------------------------------------------------------*
036200080910     c     UNISQL        BegSr
036300080910
036400080910      * Preparo la stringa sql
036500080910     c                   Eval      StringaSql =
036600080910     c                             'select * '                                 +
036700080910     c                             'from fnorm00f where ormpor'
036800080910     c* Più di un p.o.
036900080910    1c                   if        skpoel(2)>0
037000080910     c                   Eval      StringaSql =StringaSql+' in ('
037100080910     c
037200080910      * ciclo sulla sk p.o. da elaborare per i p.o. emissione
037300080910    2c                   Do        250           Xa
037400080910     c                   If        Skpoel(Xa) = *Zeros
037500080910     c                   Leave
037600080910     c                   EndIf
037700080910     c                   Eval      StringaSql =
037800080910     c                             %trim(StringaSql) +  ' ' +
037900080910     c                             %editc(Skpoel(Xa):'1') + ','
038000080910    2c                   EndDo
038100080910     c
038200080910     c*
038300080910     c                   Eval      StringaSql =
038400080910     c                             %trim(StringaSql) + ')'
038500080910     c                   Eval      StringaSql =Stringasql + ' AND ormdao='+
038600080910     c                             %editc(i48dtda:'X')
038700080910     c                   else
038800080910     c                   Eval      StringaSql =StringaSql+' =' +
038900080911     c                             %editc(skpoel(1):'X')+' and ormdao>=' +
039000080911     c                             %editc(i48dtda:'X')+ ' and ormdao<=' +
039100080910     c                             %editc(i48dtal:'X')
039200080910     c                   endif
039300080910
039400080911     c                   if        skdate(2)>0
039500080911     c                   Eval      StringaSql =StringaSql+' order by ormdao'
039600080911     c                   else
039700080911     c                   Eval      StringaSql =StringaSql+' order by ormpor'
039800080911     c                   endif
039900080910
040000080910     c                   EndSr
040100080911      *--------------------------------------------------------------*
040200080911      * SQL  - SQL con lettura di una filiale alla volta per più date
040300080911      *--------------------------------------------------------------*
040400080911     c     FILSQL        BegSr
040500080911
040600080911      * Preparo la stringa sql
040700080911     c                   Eval      StringaSql =
040800080911     c                             'select * '                                 +
040900080911     c                             'from fnorm00f where ormpor'
041000080911     c                   Eval      StringaSql =StringaSql+' =' +
041100080911     c                             %editc(wpor   :'X')+' and ormdao>=' +
041200080911     c                             %editc(i48dtda:'X')+ ' and ormdao<=' +
041300080911     c                             %editc(i48dtal:'X')
041400080911
041500080911     c                   Eval      StringaSql =StringaSql+' order by ormdao'
041600080911
041700080911     c                   EndSr
041800010614      *--------------------------------------------------------------*
041900080910      * conteggio i dati
042000010614      *--------------------------------------------------------------*
042100080910     c     Conteggio     begsr
042200080911     c
042300080911     C/EXEC SQL
042400080911     C+ prepare S1 from :StringaSql
042500080911     C/END-EXEC
042600080911
042700080911     C/EXEC SQL
042800080911     C+ declare C1 cursor for S1
042900080911     C/END-EXEC
043000080911
043100080911     C/EXEC SQL
043200080911     C+ open  C1
043300080911     C/END-EXEC
043400080911     c
043500080910     C/EXEC SQL
043600080923     C+ Fetch C1 into :fnormxxds
043700080910     C/END-EXEC
043800080910
043900080910  1  c                   Dow       Sqlcod = 0                                   *record letto
044000080910      * no gli annullati
044100080910  2  c                   If        Ormatb = *Blanks
044200080917     c* Escludo gli orm di p.o. non ancora partiti con la procedura
044300080917     c*
044400080917     c     ormpor        chain     azorg01l
044500080917     c                   clear                   wdao
044600080917     c                   if        %found(azorg01l)
044700080917     c                   movel     orgde7        og147
044800080917     c                   if        §ogddao>*zeros
044900080917     c                   movel     §ogddao       wdao
045000080917     c                   endif
045100080917     c                   endif
045200080917     c
045300080917   2ac                   if        wdao>0 and ormdao>=wdao
045400080910
045500080910      * se sono in filiale elaboro tutto, se sono in sede devo controllare anche la
045600080910      * data pulizia ORM di filiale
045700080911     c                             and (I48Sede = 'F'  or
045800080911     c                                 (I48Sede <> 'F' and Ormdao >= I48dtpo))
045900080910
046000080910      * controllo se posso elaborare la serie
046100080910     c                   Move      OrmPoe        wOrmPoe
046200080910     c                   Move      OrmNsr        wOrmNsr
046300080910     c     wosr          Lookup    skosr                                  99
046400080910    3c                   If        Not *In99
046500080910
046600080910      *
046700080910      * Rottura per data (13 on) o per filiale (13 off)
046800080910    4c                   if        (*in13 and ( ormdao<>savdao)) or
046900080911     c                             (not *in13 and ( ormpor<>savpor))
047000080910     c
047100080923    5c                   if        (savdao>0 or savpor>0)
047200080910     c                   exsr      Stampacont
047300080910    5c                   endif
047400080910     c
047500080911    5c                   if        *in13
047600080911     c                   eval      savdao=ormdao
047700080911     c                   else
047800080911     c                   eval      savpor=ormpor
047900080911     c                   eval      *in91=*on
048000080911     c                   eval      *in92=*on
048100080911    5c                   endif
048200080911     c
048300080910     c                   clear                   totA
048400080910     c                   clear                   totA1
048500080910     c                   clear                   totB
048600080910     c                   clear                   totB1
048700080910     c                   clear                   totC
048800080910     c                   clear                   totC1
048900080910    4c                   endif
049000080910     c
049100080910     c* totale ORM della filiale
049200080911     c                   add       1             totA
049300080912     c                   add       1             genA
049400080910     c     korg          chain     fnorg01l
049500080910     c* totale orm senza giro
049600080910    4c                   if        %found(fnorg01l) and orgcgi=*blanks
049700080911     c                   add       1             totA1
049800080912     c                   add       1             genA1
049900080911     c*
050000080911     c* Se richiesto stampo il dettaglio
050100080911    5c                   if        i48deta1='S'
050200080911     c                   eval      wdet='A'
050300080911     c                   exsr      Dettaglio
050400080911    5c                   endif
050500080911    4c                   endif
050600080911     c
050700080910     c* totale orm di propria competenza ritiro
050800080911    4c                   if        ormpor=ormpoe
050900080911     c                   add       1             totB
051000080912     c                   add       1             genB
051100080910     c* di cui senza mittente codificato
051200080910    5c                   if        ormcra=0
051300080911     c                   add       1             totB1
051400080912     c                   add       1             genB1
051500080911     c* Se richiesto stampo il dettaglio
051600080911    5c                   if        i48detb1='S'
051700080911     c                   eval      wdet='B'
051800080911     c                   exsr      Dettaglio
051900080911    5c                   endif
052000080910    5c                   endif
052100080910    4c                   endif
052200080910     c*
052300080910     c* totale orm con mittente codificato
052400080911    4c                   if        ormcra>0
052500080911     c                   add       1             totC
052600080912     c                   add       1             genC
052700080910     c* di cui senza giro automatico
052800080911    5c                   if        not %found(fnorg01l) or orgtgi<>'A'
052900080911     c                   add       1             totC1
053000080912     c                   add       1             genC1
053100080910    5c                   endif
053200080910    4c                   endif
053300080910     c
053400080910     c
053500080910    3c                   endif
053600080917   2ac                   endif
053700080910    2c                   EndIf
053800080910
053900080910     C/EXEC SQL
054000080911     C+ Fetch next from C1 into :fnormxxds
054100080910     C/END-EXEC
054200080910
054300080910    1c                   EndDo
054400080910
054500080910     C/EXEC SQL
054600080910     C+ close C1
054700080910     C/END-EXEC
054800080910     c                   EndSr
054900080910      *--------------------------------------------------------------*
055000080910      * STAMPA
055100080910      *--------------------------------------------------------------*
055200080910     c     Stampacont    begsr
055300010621
055400080910     ?* rottura per filiale
055500080923      * non serve per i totali generali
055600080923    0c                   if        not *in10
055700080923     c
055800080910    1c                   if        *in13 = *off
055900080910     c
056000010621      * decodifico il p.o. che sto elaborando
056100080910     c     savpor        chain     azorg01l
056200080910    2c                   if        %found(azorg01l)
056300080910     c                   movel     orgdes        despoel
056400080910    2c                   endif
056500080911     c                   movel     savpor        stapoel
056600080923     c                   movel     orgfl3        stafl3
056700080911     c
056800080911   x1c                   else
056900080911     c* giro la data in ggmmaa
057000080911
057100080911     c                   clear                   wlbdat
057200080911     c                   move      savdao        g02inv
057300080911     c                   movel     '3'           G02err
057400080911     c                   call      'XSRDA8'
057500080911     c                   parm                    wlbdat
057600080911     c                   movel     g02dat        Stadata
057700080911     c*
057800080911    1c                   endif
057900080923    0c                   endif
058000080923     c*
058100080923     c* Scrittura file se prevista
058200080923     c                   if        i48ela<>'S'
058300080923     c                   exsr      SCRIVIFILE
058400080923     c                   endif
058500080910     c
058600080923    0c                   if        i48ela<>'F' and not *in15
058700010621      * salto pagina
058800080911    1c                   if        *in90 = *on
058900080911     c                   write     or49t1
059000080911     c                   write     or49t2
059100080911     c                   write     or49t3
059200080911     c                   write     or49t4
059300010621     c                   eval      *in90 = *off
059400080923     c                   else
059500080923     c                   if        primariga=' '
059600080923     c                   write     or49t2
059700080923     c                   write     or49t3
059800080911    1c                   endif
059900080923    1c                   endif
060000080917    c
060100080917     c* Calcolo le percentuali
060200080917     c                   exsr      CalcolaPERC
060300080911     c* Dettaglio
060400080911     c                   write     or49d1
060500080911     c
060600080923     c                   eval      primariga='S'
060700080923    0c                   endif
060800080923    c
060900080911     c                   endsr
061000080910      *--------------------------------------------------------------*
061100080911      * Decodifica area di un  p.o.
061200080910      *--------------------------------------------------------------*
061300080911     c     Decodi_ARE    begsr
061400080910      * decodifico l'area
061500080910     c                   clear                   descrare
061600080910     c                   movel     '05'          tblcod
061700080910     c                   movel(P)  orgcar        tblkey
061800080910     c     ktab          chain     tabel00f
061900080910     c                   if        %found(tabel00f)
062000080910     c                   movel     tbluni        descrare
062100080910 e4  c                   endif
062200080910     c                   movel     orgcar        staare
062300080910     c                   ENDSR
062400080923      *--------------------------------------------------------------*
062500080923      * Scrittura file di work
062600080923      *--------------------------------------------------------------*
062700080923     c     SCRIVIFILE    BEGSR
062800080923     c                   clear                   wfroa000
062900080923     c
063000080923     c  n13              exsr      decodi_ARE
063100080923     c                   z-add     stapoel       roapoe
063200080923     c                   movel     despoel       roapoed
063300080923     c                   z-add     staare        roaare
063400080923     c                   movel     descrare      roaared
063500080923     c                   movel     stafl3        roadis
063600080923     c
063700080923     c   13              z-add     savdao        roadae
063800080924     c  n13              z-add     i48dtda       roadae
063900080923     c
064000080923     c                   z-add     totA          roaode
064100080923     c                   z-add     totA1         roasgr
064200080923     c                   z-add     totB          roaope
064300080923     c                   z-add     totB1         roasmi
064400080923     c                   z-add     totC          roaomc
064500080923     c                   z-add     totC1         roasga
064600080924     c
064700080924     c                   write     wfroa000                             99
064800080923     c                   ENDSR
064900010614      *--------------------------------------------------------------*
065000080911      * STAMPO IL DETTAGLIO  liste di controllo
065100010614      *--------------------------------------------------------------*
065200080911     c     Dettaglio     BEGSR
065300080911     c* Stampo testata
065400080911    1c                   if        (wdet='A' and *in91) or
065500080911     c                             (wdet='B' and *in92)
065600080911     c* Decodifico p.o. da stampare
065700080911     c                   eval      stapor=*all'?'
065800080911     c     ormpor        chain     azorg01l
065900080911    2c                   if        %found(azorg01l)
066000080911     c                   movel     orgdes        stapor
066100080911    2c                   endif
066200080911     c
066300080911    2c                   if        wdet='A'
066400080911     c                   write     or49t1c
066500080911     c                   eval      *in91 = *off
066600080911    2c                   endif
066700080911    2c                   if        wdet='B'
066800080911     c                   write     or49t1b
066900080911     c                   eval      *in92 = *off
067000080911    2c                   endif
067100080911     c
067200080911    1c                   endif
067300080911     c
067400080911     c* giro data orm
067500080911     c                   clear                   wlbdat
067600080911     c                   move      ormdao        g02inv
067700080911     c                   movel     '3'           G02err
067800080911     c                   call      'XSRDA8'
067900080911     c                   parm                    wlbdat
068000080911     c                   movel     g02dat        Stadao
068100080911     c*
068200080911     c                   movel     ormrsr        starsr
068300080912     c                   movel     ormrsr        st2rsr
068400080911     c                   movel     orminr        stainr
068500080912     c                   movel     orminr        st2inr
068600080911     c                   movel     ormlor        stalor
068700080911     C*
068800080911    1C                   IF        ormNAR=*BLANKS
068900080911     C                   EVAL      %SUBST(STALOR:21:3)=' '+ORMPRR
069000080911     c                   else
069100080911     C                   EVAL      %SUBST(STALOR:20:4)=' '+ORMnar
069200080911    1C                   endif
069300080911     c
069400080911    1c                   if        wdet='A'
069500080911     c                   write     or49d1c
069600080911    1c                   endif
069700080911    1c                   if        wdet='B'
069800080911     c                   write     or49d1b
069900080911    1c                   endif
070000080911     c
070100010614     c                   endsr
070200080912      *--------------------------------------------------------------*
070300080912      * Stmapa totale Generale
070400080912      *--------------------------------------------------------------*
070500080912     c     TotaleGEN     BEGSR
070600080923     c* Solo se c'e' e se non ho richiesto una filile e una data
070700080912     c
070800080912     c                   if        genA>0 and (i48poel=0  or skdate(2)>0)
070900080912     c                   z-add     genA          totA
071000080912     c                   z-add     genA1         totA1
071100080912     c                   z-add     genB          totB
071200080912     c                   z-add     genB1         totB1
071300080912     c                   z-add     genC          totC
071400080912     c                   z-add     genC1         totC1
071500080917     c* Calcola percentuali
071600080917     c                   exsr      CalcolaPERC
071700080912      * salto pagina
071800080912    1c                   if        *in90 = *on
071900080912     c                   write     or49t1
072000080912     c                   write     or49t2
072100080912     c                   write     or49t3
072200080912     c                   write     or49t4
072300080912     c                   eval      *in90 = *off
072400080912    1c                   endif
072500080912     c
072600080912     c*
072700080923     c  n16              write     or49d2
072800080912     c                   write     or49d1
072900080923     c  n16              write     or49d2
073000080912     c                   setoff                                       10
073100080912     c                   endif
073200080912     c
073300080912     c                   ENDSR
073400080917      *--------------------------------------------------------------*
073500080917      * calcola percentuali dei totale
073600080917      *--------------------------------------------------------------*
073700080917     c     CalcolaPERC   BEGSR
073800080917     c                   clear                   percA1
073900080917     c                   clear                   percB1
074000080917     c                   clear                   percC1
074100080917     c                   if        totA>0
074200080917     c                   eval(H)   perca1=(tota1*100)/totA
074300080917     c                   endif
074400080917     c                   if        totB>0
074500080917     c                   eval(H)   percb1=(totb1*100)/totB
074600080917     c                   endif
074700080917     c                   if        totC>0
074800080917     c                   eval(H)   percc1=(totc1*100)/totC
074900080917     c                   endif
075000080917     c                   ENDSR
075100010604      *--------------------------------------------------------------*
075200010604      * ROUTINE INIZIALE
075300010604      *--------------------------------------------------------------*
075400010604     c     *inzsr        begsr
075500010604      *
075600010604     c     *entry        plist
075700010604     c                   parm                    kpjba
075800010621
075900080908     c                   movel     kpjbu         fior48ds
076000010621
076100080908     c                   z-add     1             codut             1 0
076200080908      *
076300080908     c     *dtaara       define    §azute        azuteds
076400080908     c     *dtaara       define    §datiute      ddatiute
076500080908     c                   in(E)     *dtaara
076600080908    1c                   If        %error  or RSUT = *blanks
076700080908     c                   Clear                   Tibs34ds
076800080908     c                   Call      'TIBS34R'
076900080908     c                   Parm                    Tibs34ds
077000080908     c                   In        *dtaara
077100080908    1c                   EndIf
077200080908
077300080908     c                   Clear                   wabi
077400080908     c                   Clear                   dLat
077500080908
077600080908      * Verifica errori e autorità profilo
077700080908s   1c                   Select
077800080908      * se ho errori nei dati utente esco dal pgm
077900080908w   1c                   When      DutErr = 'E'
078000080908     c                   clear                   wabi
078100080908      * se non c'è l'abilitazione
078200080908      * --> se 1° livello, abilitazioni al terminal
078300080908      *     se 2° livello, abilitazioni al punto operativo
078400080908      *     se sede è impossibile ma se capita mando a fine il pgm
078500080908w   1c                   When      UteAut = *Blanks
078600080908if  2c                   If        DutLpo = '1'
078700080908     c                   Eval      wabi   = 'TP'
078800080908e   2c                   EndIf
078900080908if  2c                   If        DutLpo = '2'
079000080908     c                   Eval      wabi   = 'PO'
079100080908e   2c                   EndIf
079200080908if  2c                   If        DutLpo = 'S'
079300080908     c                   eval      duterr='E'
079400080908     c                   clear                   wabi
079500080908e   2c                   EndIf
079600080908      * carica le abilitazioni del profilo
079700080908x   1c                   Other
079800080908     c                   Eval      wabi = UteAut
079900080908e   1c                   EndSl
080000080908
080100080908      * controllo se ok l'abilitazione dell'utente
080200080908     c                   Clear                   Tibs02ds
080300080908     c                   Eval      T02Mod = 'C'
080400080908     c                   Eval      T02Sif = knsif
080500080908     c                   Eval      T02Cod = 'LAT'
080600080908     c                   Movel(p)  wabi          T02Ke1
080700080908     c                   Call      'TIBS02R'
080800080908     c                   Parm                    kpjba
080900080908     c                   Parm                    Tibs02ds
081000080908if  1c                   If        T02Err = *Blanks
081100080908     c                   Eval      dLat = T02Uni
081200080908e   1c                   EndIf
081300080908      * errore o non abilitato
081400080908if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
081500080908   x1c                   Else
081600080908      * Reperimento dei P.O. gestibili dall'utente
081700080908     c                   clear                   TRUL31DS
081800080908     c                   clear                   TRUL31DS2
081900080908     c                   eval      I31abi = wabi
082000080908     c                   eval      I31cdi = DUTdis
082100080908     c                   eval      I31car = DUTare
082200080908     c                   eval      I31cpo = DUTpou
082300080908     c                   call      'TRUL31R'
082400080908     c                   parm                    KPJBA
082500080908     c                   parm                    TRUL31DS
082600080908     c                   parm                    TRUL31DS2
082700080908if  2c                   if        O31pog > *zeros
082800080908     c                   movea     O31pog        $pog
082900080908     c                   movea     O31arg        $arg
083000080908     c                   movea     O31dig        $dig
083100080908     c                   else
083200080908     c                   clear                   $pog
083300080911    2c                   endif
083400080911    1c                   endif
083500080908
083600010604      * reperisco data e ora
083700010604     c                   time                    w0140
083800010604      * udate in GGMMAAAA
083900010604     c                   move      w0140         wdate
084000010604     c                   movel     w0140         wora
084100010608
084200010608      * pulisco le sk
084300010608     c                   clear                   kfile
084400030916     c                   clear                   kfile1
084500010608     c                   clear                   rfile
084600010608     c                   clear                   skdate
084700010608     c                   clear                   skpoel
084800031003     c                   Clear                   skosr
084900010621
085000010621      * controllo se devo stampare per data o per p.o.
085100080910      * 13 off - stampo data in testata e dettaglio per p.o.
085200080910      * 13 on  - stampo p.o. in testata e dettaglio per data
085300080911     c                   if        i48poel = 0   and  (i48dtal=0 or
085400080910     c                             i48dtda =  i48dtal)
085500080910     c                   eval      *in13 = *off
085600010621     c                   else
085700080910     c                   eval      *in13 = *on
085800080923     c
085900080923     c* Se stampa sintetica per tante date stampo in testata
086000080923     c                   if        i48tip='S' AND i48poel=0 and i48dtal>0
086100080923     c                             and i48dtda<>i48dtal
086200080923     c                   eval      *in13 = *off
086300010621     c                   endif
086400080923     c
086500080923     c                   endif
086600010608
086700010608      * carico la sk delle date da elaborare
086800010703     c                   eval      xd = 1
086900080911     c                   eval      skdate(xd) = i48dtda
087000080911     c                   if        i48dtda <> i48dtal
087100080911     c                   eval      wdtgio = i48dtda
087200080911     c     *iso          movel     i48dtda       wdtda
087300080911     c                   dow       wdtgio < i48dtal
087400010703     c                   eval      xd = xd +1
087500010703     c                   adddur    1:*d          wdtda
087600010703     c                   move      wdtda         wdtgio
087700010703     c                   eval      skdate(xd) = wdtgio
087800010703     c                   enddo
087900010703     c                   endif
088000020807
088100020808     c                   z-add     1             xa
088200010608
088300080909      * Elaboro tutti i p.o. A CUI L'UTENTE è abilitato
088400080911 b1  c                   if        i48poel = 0
088500080909     c                   dow       $pog(Xa)>*zeros
088600080909     c                   movel     $pog(Xa)      skpoel(Xa)
088700080909     c                   add       1             xa
088800080909     c                   enddo
088900020807 e1  c                   EndIf
089000010605      * lanciato un solo po carico questo nella sk
089100080911     c                   if        i48poel  > 0
089200080909     c                   eval      skpoel(1) = i48poel
089300010605     c                   endif
089400031002
089500031002      * carico la sk delle serie da non elaborare in statistica
089600031002     c                   Eval      xc = 1
089700031002     c                   Eval      TbeCod = 'OSR'
089800031002     c     TbeCod        Setll     Tntbe01l
089900031002Do  1c                   Do        *Hival
090000031002     c     TbeCod        Reade     Tntbe01l
090100031002     c                   If        %Eof(Tntbe01l)
090200031002     c                   Leave
090300031002     c                   EndIf
090400031002     c                   Movel     TbeUni        dOsr
090500031002     c                   If        d§OsrSta = *Blanks
090600031002     c                   Iter
090700031002     c                   EndIf
090800031002     c                   Movel     TbeKe1        SkOsr(xc)
090900031002     c                   Add       1             Xc
091000031002    1c                   EndDo
091100080910     c
091200080911     c                   setoff                                       10
091300010620
091400010705      * se richiesta la stampa di controllo apro il file di stampa
091500080909     c                   if        i48DETA1= 'S'
091600080909     c                   open      fior49p1
091700010620     c                   endif
091800080911      * se richiesta la stampa di controllo apro il file di stampa
091900080911     c                   if        i48DETB1= 'S'
092000080911     c                   open      fior49p2
092100080911     c                   endif
092200080911     c
092300080911     c                   eval      *in90=*on
092400080911     c                   eval      *in91=*on
092500080911     c                   eval      *in92=*on
092600010605
092700010621      * se richiesta la creazione del file lo apro
092800080911     c***                if        i48tpel = 'F'
092900080909     c***                open      wfror01l
093000080909     c***                endif
093100010621
093200010605      * chiavi
093300080910     c     korg          klist
093400080910     c                   kfld                    ormpoe
093500080910     c                   kfld                    ormnsr
093600080910     c                   kfld                    ormnor
093700080910     c                   kfld                    ormnrv
093800010606
093900010620     c     ktab          klist
094000010620     c                   kfld                    codut
094100010620     c                   kfld                    tblcod
094200010620     c                   kfld                    tblkey
094300010620
094400010611     c     ktntbe        klist
094500010611     c                   kfld                    tbecod
094600010611     c                   kfld                    tbeke1
094700080924
094800080924     c     kroa          klist
094900080924     c                   kfld                    kroapoe
095000080924     c                   kfld                    kroadta
095100010606
095200010604     c                   endsr
