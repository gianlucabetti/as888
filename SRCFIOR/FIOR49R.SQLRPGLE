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
004400130121     d wORMcor         s              7s 0 inz
004500010605
004600080910     d savdao          s                   like(ormdao) inz
004700080910     d savpor          s                   like(ormpor) inz
004800080924     d
004900080924     d kroapoe         s                   like(roapoe)
005000080924     d kroadta         s                   like(roadae)
005100010607
005200010612     d xd              s              5i 0 inz(*zeros)                          indice sk date
005300010612     d xa              s              5i 0 inz(*zeros)                          indice sk po da ela.
005400020807     d Xb              s              5i 0 inz(*zeros)                          indice sk po TRUL26
005500130121     d******* xc              s              5i 0 inz(*zeros)                          indice sk OSR
005600080912     d genA            s              7  0 inz(*zeros)                          indice sk OSR
005700080912     d genA1           s              7  0 inz(*zeros)                          indice sk OSR
005800080912     d genB            s              7  0 inz(*zeros)                          indice sk OSR
005900080912     d genB1           s              7  0 inz(*zeros)                          indice sk OSR
006000080912     d genC            s              7  0 inz(*zeros)                          indice sk OSR
006100080912     d genC1           s              7  0 inz(*zeros)                          indice sk OSR
006200080923     d finA            s              7  0 inz(*zeros)                          indice sk OSR
006300080923     d finA1           s              7  0 inz(*zeros)                          indice sk OSR
006400080923     d finB            s              7  0 inz(*zeros)                          indice sk OSR
006500080923     d finB1           s              7  0 inz(*zeros)                          indice sk OSR
006600080923     d finC            s              7  0 inz(*zeros)                          indice sk OSR
006700080923     d finC1           s              7  0 inz(*zeros)                          indice sk OSR
006800020808
006900080912     d StringaSql      s           4096    Varying
007000010605
007100010608     d wdtda           s               d   datfmt(*iso)
007200010608     d wdtal           s               d   datfmt(*iso)
007300031002
007400010605
007500010605      *   S C H I E R E
007600010605
007700010608     d skdate          s              8  0 dim(31)                              date da elab.
007800020807     d skpoel          s              3  0 dim(250)                             po da elab.
007900080911     D $pog            s              3    dim(250)
008000080911     D $DIG            s              1    dim(20)
008100080911     D $ARG            s              3    dim(50)
008200080911     d
008300030912     d kfile           s             12    dim(18000)                           file (key)
008400010605     d                                     varying
008500010606     d                                     inz
008600030912     d kfile1          s             11    dim(18000)                           file (key) appoggio
008700030912     d                                     varying
008800030912     d                                     inz
008900030912     d rfile           s            456    dim(18000)                           file (record)
009000010605     d                                     varying
009100010606     d                                     inz
009200031002
009300130121     d** skosr           s              5    dim(999)                             serie da non elab.
009400010604
009500010604      *   D S   I N T E R N E / E S T E R N E
009600010605
009700010604     d wlbdat          ds                  inz
009800010604     d  g02dat                 1      8  0
009900010604     d  g02inv                 9     16  0
010000010604     d  g02err                17     17
010100010604     d  g02tgi                18     22  0
010200031002
010300130121     d***** wosr            ds
010400130121     d*****  wOrmPoe                1      3
010500130121     d*****  wOrmNsr                4      5
010600010604
010700080908     d fior48ds        ds                  inz
010800080908     d  I48poel                1      3  0
010900080908     d  I48dtda                4     11  0
011000080908     d  I48dtal               12     19  0
011100080908     d  I48deta1              20     20
011200080908     d  I48detb1              21     21
011300080908     d  I48dtpo               22     29  0
011400080908     d  I48Sede               30     30
011500080923     d  I48tip                31     31
011600080923     d  I48ela                32     32
011700080908
011800080908     D TIBS02DS      E DS
011900080908     D TRUL31DS      E DS
012000080908     D TRUL31DS2     E DS
012100080908     D TIBS34DS      E DS
012200080908     d Azuteds       e ds                  extname(Azute00f)
012300080908     d dDatiute      e ds
012400080908     d dLat          e ds
012500080908     d dute01        e ds
012600080917     d og147         e ds
012700080908
012800031002     d dosr          e ds
012900020807
013000020807     d FnormxxDs     e ds                  ExtName(Fnorm00f)
013100020807
013200010604     d kpjba         e ds
013300010604      *--------------------------------------------------------------*
013400010604
013500080910      * Stampo le selezioni
013600010604     c                   exsr      PRESELEZ
013700080910      *
013800080909      * Se richiesta una data o una filiale, faccio un solo SQL
013900080910      *  altrimenti per ogni data elaboro tutti i p.o.
014000080909     c                   if        skdate(2)=0 or skpoel(2)=0
014100080910     c                   exsr      UNiSQL
014200080911     c
014300080911     c                   exsr      Conteggio
014400080911     c
014500080911     c* ultima riga di stampa e fine stampa
014600080924     c                   if        totA>0
014700080911     c                   exsr      Stampacont
014800080911     c                   endif
014900080911
015000080912     c* totale generale
015100080924     c                   if        i48ela<>'F'
015200080923     c                   seton                                        10
015300080912     c                   EXSR      TotaleGEN
015400080911      * fine stampa
015500080911     c                   write     or49e1
015600080924     c                   endif
015700080911     c
015800080909     c                   else
015900080911     c* loop sulle filiale per stamparne una alla volta per tutte le date
016000080911     c                   z-add     1             xa
016100080911     c                   dow       skpoel(xa)>0
016200080911     c                   z-add     skpoel(Xa)    wpor
016300080911     c
016400080911     c                   movel     wpor          stapoel
016500080911     c     wpor          chain     azorg01l
016600080911     c                   if        %found(azorg01l)
016700080911     c                   movel     orgdes        despoel
016800080923     c                   movel     orgfl3        stafl3
016900080911     c                   exsr      DECODI_ARE
017000080911     c                   else
017100080911     c                   eval      despoel=*all'?'
017200080911     c                   endif
017300080923     c                   clear                   primariga
017400080911     c
017500080911     c
017600080911     c                   exsr      filSQL
017700080911     c
017800080911     c                   exsr      Conteggio
017900080911     c
018000080911     c* ultima riga di stampa e fine stampa
018100080924     c                   if        totA>0
018200080911     c                   exsr      Stampacont
018300080911     c                   endif
018400080924     c
018500080923     c                   if        totA>0 and *in15
018600080924     c                   if        i48ela<>'F'
018700080923     c* Accenfo indicatore 16 per non stampare le righe di sepaerazione
018800080923     c                   seton                                        16
018900080923     c                   EXSR      TotaleGEN
019000080923     c                   setoff                                       16
019100080924     c                   endif
019200080924     c
019300080923     c* mi salvo i totali generali della filiale per fare il totalone
019400080923     c*  alla fine
019500080923     c                   add       genA          finA
019600080923     c                   add       genA1         finA1
019700080923     c                   add       genB          finB
019800080923     c                   add       genB1         finB1
019900080923     c                   add       genC          finC
020000080923     c                   add       genC1         finC1
020100080923     c
020200080923     c                   clear                   genA
020300080923     c                   clear                   genA1
020400080923     c                   clear                   genB
020500080923     c                   clear                   genB1
020600080923     c                   clear                   genC
020700080923     c                   clear                   genC1
020800080923     c                   endif
020900080911     c
021000080911     c                   eval      *in91=*on
021100080911     c                   eval      *in92=*on
021200080923     c
021300080911     c                   clear                   savpor
021400080911     c                   clear                   savdao
021500080911     c
021600080911     c                   clear                   totA
021700080911     c                   clear                   totA1
021800080911     c                   clear                   totB
021900080911     c                   clear                   totB1
022000080911     c                   clear                   totC
022100080911     c                   clear                   totC1
022200080911
022300080911     c                   add       1             xa
022400080911     c                   enddo
022500080911     c
022600080912     c* totale generale
022700080923     c* Se stampa sintetica nuovo il totale finale
022800080924     c                   if        i48ela<>'F'
022900080923     c                   if        *in15
023000080923     c                   z-add     finA          genA
023100080923     c                   z-add     finA1         genA1
023200080923     c                   z-add     finB          genB
023300080923     c                   z-add     finB1         genB1
023400080923     c                   z-add     finC          genC
023500080923     c                   z-add     finC1         genC1
023600080923     c*
023700080923     c                   setoff                                       15
023800080923     c                   endif
023900080923     c
024000080923     c                   seton                                        10
024100080912     c                   EXSR      TotaleGEN
024200080911      * fine stampa
024300080911     c                   write     or49e1
024400080911     c*
024500080909     c                   endif
024600080924     c                   endif
024700010605
024800010608      * fine programma
024900010605     c                   eval      *inlr = *on
025000010605
025100010604      *--------------------------------------------------------------*
025200010604      * PREPARA LA STAMPA DELLA VIDEATA DELLE SELEZIONI
025300010604      *--------------------------------------------------------------*
025400010604     c     PRESELEZ      begsr
025500010618
025600010618      * stampo la prima pagina con le selezioni
025700080911     c                   eval      *in12 = (i48sede = 'S')
025800010618
025900080910     c                   if        i48poel = 0
026000080909     c                   eval      despo = 'Tutte le fil.abilitate'
026100010618     c                   else
026200080911     c     i48poel       chain     azorg01l
026300010618     c                   if        %found(azorg01l)
026400010618     c                   movel     orgdes        despo
026500080910     c                   movel     orgdes        despoel
026600080911     c                   movel     i48poel       stapoel
026700080923     c                   movel     orgfl3        stafl3
026800080910     c
026900080911     c* DEcodifica area
027000080911     c                   exsr      decodi_ARE
027100010618     c                   endif
027200010618     c                   endif
027300080923     c
027400080923     c* decodifica tipo stampa
027500080923     c                   if        i48tip='S'
027600080923     c                   eval      statipo='SINTETICA'
027700080923     c                   seton                                        15
027800080923     c* se una data solo allora stampo come se fosse analitica
027900080923     c*  perchè non c'e' differenza  con la sintetica
028000080923     c                   if        i48tip='S' AND  (i48dtal=0
028100080923     c                             or i48dtda=i48dtal)
028200080923     c                   eval      *in15 = *off
028300080923     c                   endif
028400080923     c
028500080923     c                   else
028600080923     c                   eval      statipo='ANALITICA'
028700080923     c                   setoff                                       15
028800080923     c                   endif
028900080924     c* decodifica tipo elaborazione
029000080924     c                   select
029100080924     c                   WHEN      i48ela='S'
029200080924     c                   eval      staela='SOLO STAMPA'
029300080924
029400080924     c                   WHEN      i48ela='F'
029500080924     c                   eval      staela='SOLO FILE  '
029600080924     C
029700080924     c                   OTHER
029800080924     c                   eval      staela='STAMPA E CREAZIONE FILE'
029900080924     C                   ENDSL
030000010618
030100010618     c                   clear                   wlbdat
030200080911     c                   move      i48dtda       g02inv
030300010618     c                   movel     '3'           G02err
030400010618     c                   call      'XSRDA8'
030500010618     c                   parm                    wlbdat
030600010618     c                   movel     g02dat        datada
030700010618
030800010618     c                   clear                   wlbdat
030900080911     c                   move      i48dtal       g02inv
031000010618     c                   movel     '3'           G02err
031100010618     c                   call      'XSRDA8'
031200010618     c                   parm                    wlbdat
031300010618     c                   movel     g02dat        dataal
031400080911     c                   write     or49s1
031500010618
031600080911     c                   if        i48deta1='S'
031700080911     c                   write     or49s1c
031800080911     c                   endif
031900080911     c
032000080911     c                   if        i48detb1='S'
032100080911     c                   write     or49s1b
032200080911     c                   endif
032300080911     c
032400080924     c* Se devo creare il file prima pulisco se già ci sono i record
032500080924     c                   if        i48ela <>'S'
032600080924     c                   open      wfroa01l
032700080924
032800080924     c                   eval      xa = 1
032900080924     c                   do        250           xa
033000080924     c                   if        skpoel(xa) = *zeros
033100080924     c                   leave
033200080924     c                   endif
033300080924
033400080924     c     *iso          movel     i48dtda       wdtda
033500080924     c                   if        i48dtal>0
033600080924     c     *iso          movel     i48dtal       wdtal
033700080924     c                   else
033800080924     c     *iso          movel     i48dtda       wdtal
033900080924     c                   endif
034000080924
034100080924     c                   dow       wdtda<=wdtal
034200080924     c                   eval      kroapoe = skpoel(xa)
034300080924     c                   move      wdtda         kroadta
034400080924     c     kroa          setll     wfroa01l
034500080924     c                   if        %equal(wfroa01l)
034600080924     c     kroa          reade     wfroa01l
034700080924     c                   dow       not%eof(wfroa01l)
034800080924     c                   delete    wfroa000
034900080924     c     kroa          reade     wfroa01l
035000080924     c                   enddo
035100080924     c                   endif
035200080924     c
035300080924     c                   adddur    1:*d          wdtda
035400080924     c                   enddo
035500080924
035600080924     c                   enddo
035700080924     c                   endif
035800080924
035900010604     c                   endsr
036000080910      *--------------------------------------------------------------*
036100080910      * SQL  - SQL unico
036200080910      *--------------------------------------------------------------*
036300080910     c     UNISQL        BegSr
036400080910
036500080910      * Preparo la stringa sql
036600080910     c                   Eval      StringaSql =
036700080910     c                             'select * '                                 +
036800080910     c                             'from fnorm00f where ormpor'
036900080910     c* Più di un p.o.
037000080910    1c                   if        skpoel(2)>0
037100080910     c                   Eval      StringaSql =StringaSql+' in ('
037200080910     c
037300080910      * ciclo sulla sk p.o. da elaborare per i p.o. emissione
037400080910    2c                   Do        250           Xa
037500080910     c                   If        Skpoel(Xa) = *Zeros
037600080910     c                   Leave
037700080910     c                   EndIf
037800080910     c                   Eval      StringaSql =
037900080910     c                             %trim(StringaSql) +  ' ' +
038000080910     c                             %editc(Skpoel(Xa):'1') + ','
038100080910    2c                   EndDo
038200080910     c
038300080910     c*
038400080910     c                   Eval      StringaSql =
038500080910     c                             %trim(StringaSql) + ')'
038600080910     c                   Eval      StringaSql =Stringasql + ' AND ormdao='+
038700080910     c                             %editc(i48dtda:'X')
038800080910     c                   else
038900080910     c                   Eval      StringaSql =StringaSql+' =' +
039000080911     c                             %editc(skpoel(1):'X')+' and ormdao>=' +
039100080911     c                             %editc(i48dtda:'X')+ ' and ormdao<=' +
039200080910     c                             %editc(i48dtal:'X')
039300080910     c                   endif
039400080910
039500080911     c                   if        skdate(2)>0
039600080911     c                   Eval      StringaSql =StringaSql+' order by ormdao'
039700080911     c                   else
039800080911     c                   Eval      StringaSql =StringaSql+' order by ormpor'
039900080911     c                   endif
040000080910
040100080910     c                   EndSr
040200080911      *--------------------------------------------------------------*
040300080911      * SQL  - SQL con lettura di una filiale alla volta per più date
040400080911      *--------------------------------------------------------------*
040500080911     c     FILSQL        BegSr
040600080911
040700080911      * Preparo la stringa sql
040800080911     c                   Eval      StringaSql =
040900080911     c                             'select * '                                 +
041000080911     c                             'from fnorm00f where ormpor'
041100080911     c                   Eval      StringaSql =StringaSql+' =' +
041200080911     c                             %editc(wpor   :'X')+' and ormdao>=' +
041300080911     c                             %editc(i48dtda:'X')+ ' and ormdao<=' +
041400080911     c                             %editc(i48dtal:'X')
041500080911
041600080911     c                   Eval      StringaSql =StringaSql+' order by ormdao'
041700080911
041800080911     c                   EndSr
041900010614      *--------------------------------------------------------------*
042000080910      * conteggio i dati
042100010614      *--------------------------------------------------------------*
042200080910     c     Conteggio     begsr
042300080911     c
042400080911     C/EXEC SQL
042500080911     C+ prepare S1 from :StringaSql
042600080911     C/END-EXEC
042700080911
042800080911     C/EXEC SQL
042900080911     C+ declare C1 cursor for S1
043000080911     C/END-EXEC
043100080911
043200080911     C/EXEC SQL
043300080911     C+ open  C1
043400080911     C/END-EXEC
043500080911     c
043600080910     C/EXEC SQL
043700080923     C+ Fetch C1 into :fnormxxds
043800080910     C/END-EXEC
043900080910
044000080910  1  c                   Dow       Sqlcod = 0                                   *record letto
044100080910      * no gli annullati
044200080910  2  c                   If        Ormatb = *Blanks
044300080917     c* Escludo gli orm di p.o. non ancora partiti con la procedura
044400080917     c*
044500080917     c     ormpor        chain     azorg01l
044600080917     c                   clear                   wdao
044700080917     c                   if        %found(azorg01l)
044800080917     c                   movel     orgde7        og147
044900080917     c                   if        §ogddao>*zeros
045000080917     c                   movel     §ogddao       wdao
045100080917     c                   endif
045200080917     c                   endif
045300080917     c
045400080917   2ac                   if        wdao>0 and ormdao>=wdao
045500080910
045600080910      * se sono in filiale elaboro tutto, se sono in sede devo controllare anche la
045700080910      * data pulizia ORM di filiale
045800080911     c                             and (I48Sede = 'F'  or
045900080911     c                                 (I48Sede <> 'F' and Ormdao >= I48dtpo))
046000080910
046100080910      * controllo se posso elaborare la serie
046200130121      * da Gennaio 2013 il controllo sulla tabella OSR lo faccio per
046300130121      * codice ordinante
046400130121     c                   eval      *in99 = *off
046500130121     c                   IF        ORMnsr > 0
046600130121     c                   exsr      sr_tabosr
046700130121     c                   ENDIF
046800130121     c********           Move      OrmPoe        wOrmPoe
046900130121     c********           Move      OrmNsr        wOrmNsr
047000130121     c*****wosr          Lookup    skosr                                  99
047100080910    3c                   If        Not *In99
047200080910
047300080910      *
047400080910      * Rottura per data (13 on) o per filiale (13 off)
047500080910    4c                   if        (*in13 and ( ormdao<>savdao)) or
047600080911     c                             (not *in13 and ( ormpor<>savpor))
047700080910     c
047800080923    5c                   if        (savdao>0 or savpor>0)
047900080910     c                   exsr      Stampacont
048000080910    5c                   endif
048100080910     c
048200080911    5c                   if        *in13
048300080911     c                   eval      savdao=ormdao
048400080911     c                   else
048500080911     c                   eval      savpor=ormpor
048600080911     c                   eval      *in91=*on
048700080911     c                   eval      *in92=*on
048800080911    5c                   endif
048900080911     c
049000080910     c                   clear                   totA
049100080910     c                   clear                   totA1
049200080910     c                   clear                   totB
049300080910     c                   clear                   totB1
049400080910     c                   clear                   totC
049500080910     c                   clear                   totC1
049600080910    4c                   endif
049700080910     c
049800080910     c* totale ORM della filiale
049900080911     c                   add       1             totA
050000080912     c                   add       1             genA
050100080910     c     korg          chain     fnorg01l
050200080910     c* totale orm senza giro
050300080910    4c                   if        %found(fnorg01l) and orgcgi=*blanks
050400080911     c                   add       1             totA1
050500080912     c                   add       1             genA1
050600080911     c*
050700080911     c* Se richiesto stampo il dettaglio
050800080911    5c                   if        i48deta1='S'
050900080911     c                   eval      wdet='A'
051000080911     c                   exsr      Dettaglio
051100080911    5c                   endif
051200080911    4c                   endif
051300080911     c
051400080910     c* totale orm di propria competenza ritiro
051500080911    4c                   if        ormpor=ormpoe
051600080911     c                   add       1             totB
051700080912     c                   add       1             genB
051800080910     c* di cui senza mittente codificato
051900080910    5c                   if        ormcra=0
052000080911     c                   add       1             totB1
052100080912     c                   add       1             genB1
052200080911     c* Se richiesto stampo il dettaglio
052300080911    5c                   if        i48detb1='S'
052400080911     c                   eval      wdet='B'
052500080911     c                   exsr      Dettaglio
052600080911    5c                   endif
052700080910    5c                   endif
052800080910    4c                   endif
052900080910     c*
053000080910     c* totale orm con mittente codificato
053100080911    4c                   if        ormcra>0
053200080911     c                   add       1             totC
053300080912     c                   add       1             genC
053400080910     c* di cui senza giro automatico
053500080911    5c                   if        not %found(fnorg01l) or orgtgi<>'A'
053600080911     c                   add       1             totC1
053700080912     c                   add       1             genC1
053800080910    5c                   endif
053900080910    4c                   endif
054000080910     c
054100080910     c
054200080910    3c                   endif
054300080917   2ac                   endif
054400080910    2c                   EndIf
054500080910
054600080910     C/EXEC SQL
054700080911     C+ Fetch next from C1 into :fnormxxds
054800080910     C/END-EXEC
054900080910
055000080910    1c                   EndDo
055100080910
055200080910     C/EXEC SQL
055300080910     C+ close C1
055400080910     C/END-EXEC
055500080910     c                   EndSr
055600130121
055700130121      /free
055800130121       //--------------------------------------------------------------
055900130121       //?Controllo tabella OSR
056000130121       //--------------------------------------------------------------
056100130121       BEGSR sr_TABosr;
056200130121
056300130121       //?Controllo se il cliente è in tabella OSR
056400130121         *in99 = *off;
056500130121         clear dOSR;
056600130121         wORMcor = %dec(%subst(%editc(ORMcor:'X'):1:7):7:0);
056700130121       //?leggo tutta la tabella OSR
056800130121         TBEcod = 'OSR';
056900130121         setll TBEcod tntbe01l;
057000130121         reade TBEcod tntbe01l;
057100130121
057200130121         DOW not %eof(tntbe01l);
057300130122           IF  TBEatb = *blanks;
057400130121           dosr = tbeuni;
057500130121           IF d§osrcli  = wORMcor or d§OSRcl2  = wORMcor or
057600130121              d§osrcl3  = wORMcor or d§OSRcl4  = wORMcor or
057700130121              d§osrcl5  = wORMcor or d§OSRcl6  = wORMcor or
057800130121              d§osrcl7  = wORMcor or d§OSRcl8  = wORMcor or
057900130121              d§osrcl9  = wORMcor or d§OSRcl10 = wORMcor or
058000130121              d§osrcl11 = wORMcor or d§OSRcl12 = wORMcor or
058100130121              d§osrcl13 = wORMcor or d§OSRcl14 = wORMcor or
058200130121              d§osrcl15 = wORMcor or d§OSRcl16 = wORMcor;
058300130121             IF  d§OSRsta <> *blanks;
058400130121               *in99 = *on;
058500130121             ENDIF;
058600130121             leavesr;
058700130121           ENDIF;
058800130122           ENDIF;
058900130121           reade TBEcod tntbe01l;
059000130121         ENDDO;
059100130121
059200130121       //?se arrivo qua vuol dire che non ho trovato il cliente in tabella OSR
059300130121         *in99 = *on;
059400130121
059500130121       ENDSR;
059600130121      /end-free
059700130121
059800080910      *--------------------------------------------------------------*
059900080910      * STAMPA
060000080910      *--------------------------------------------------------------*
060100080910     c     Stampacont    begsr
060200010621
060300080910     ?* rottura per filiale
060400080923      * non serve per i totali generali
060500080923    0c                   if        not *in10
060600080923     c
060700080910    1c                   if        *in13 = *off
060800080910     c
060900010621      * decodifico il p.o. che sto elaborando
061000080910     c     savpor        chain     azorg01l
061100080910    2c                   if        %found(azorg01l)
061200080910     c                   movel     orgdes        despoel
061300080910    2c                   endif
061400080911     c                   movel     savpor        stapoel
061500080923     c                   movel     orgfl3        stafl3
061600080911     c
061700080911   x1c                   else
061800080911     c* giro la data in ggmmaa
061900080911
062000080911     c                   clear                   wlbdat
062100080911     c                   move      savdao        g02inv
062200080911     c                   movel     '3'           G02err
062300080911     c                   call      'XSRDA8'
062400080911     c                   parm                    wlbdat
062500080911     c                   movel     g02dat        Stadata
062600080911     c*
062700080911    1c                   endif
062800080923    0c                   endif
062900080923     c*
063000080923     c* Scrittura file se prevista
063100080923     c                   if        i48ela<>'S'
063200080923     c                   exsr      SCRIVIFILE
063300080923     c                   endif
063400080910     c
063500080923    0c                   if        i48ela<>'F' and not *in15
063600010621      * salto pagina
063700080911    1c                   if        *in90 = *on
063800080911     c                   write     or49t1
063900080911     c                   write     or49t2
064000080911     c                   write     or49t3
064100080911     c                   write     or49t4
064200010621     c                   eval      *in90 = *off
064300080923     c                   else
064400080923     c                   if        primariga=' '
064500080923     c                   write     or49t2
064600080923     c                   write     or49t3
064700080911    1c                   endif
064800080923    1c                   endif
064900080917    c
065000080917     c* Calcolo le percentuali
065100080917     c                   exsr      CalcolaPERC
065200080911     c* Dettaglio
065300080911     c                   write     or49d1
065400080911     c
065500080923     c                   eval      primariga='S'
065600080923    0c                   endif
065700080923    c
065800080911     c                   endsr
065900080910      *--------------------------------------------------------------*
066000080911      * Decodifica area di un  p.o.
066100080910      *--------------------------------------------------------------*
066200080911     c     Decodi_ARE    begsr
066300080910      * decodifico l'area
066400080910     c                   clear                   descrare
066500080910     c                   movel     '05'          tblcod
066600080910     c                   movel(P)  orgcar        tblkey
066700080910     c     ktab          chain     tabel00f
066800080910     c                   if        %found(tabel00f)
066900080910     c                   movel     tbluni        descrare
067000080910 e4  c                   endif
067100080910     c                   movel     orgcar        staare
067200080910     c                   ENDSR
067300080923      *--------------------------------------------------------------*
067400080923      * Scrittura file di work
067500080923      *--------------------------------------------------------------*
067600080923     c     SCRIVIFILE    BEGSR
067700080923     c                   clear                   wfroa000
067800080923     c
067900080923     c  n13              exsr      decodi_ARE
068000080923     c                   z-add     stapoel       roapoe
068100080923     c                   movel     despoel       roapoed
068200080923     c                   z-add     staare        roaare
068300080923     c                   movel     descrare      roaared
068400080923     c                   movel     stafl3        roadis
068500080923     c
068600080923     c   13              z-add     savdao        roadae
068700080924     c  n13              z-add     i48dtda       roadae
068800080923     c
068900080923     c                   z-add     totA          roaode
069000080923     c                   z-add     totA1         roasgr
069100080923     c                   z-add     totB          roaope
069200080923     c                   z-add     totB1         roasmi
069300080923     c                   z-add     totC          roaomc
069400080923     c                   z-add     totC1         roasga
069500080924     c
069600080924     c                   write     wfroa000                             99
069700080923     c                   ENDSR
069800010614      *--------------------------------------------------------------*
069900080911      * STAMPO IL DETTAGLIO  liste di controllo
070000010614      *--------------------------------------------------------------*
070100080911     c     Dettaglio     BEGSR
070200080911     c* Stampo testata
070300080911    1c                   if        (wdet='A' and *in91) or
070400080911     c                             (wdet='B' and *in92)
070500080911     c* Decodifico p.o. da stampare
070600080911     c                   eval      stapor=*all'?'
070700080911     c     ormpor        chain     azorg01l
070800080911    2c                   if        %found(azorg01l)
070900080911     c                   movel     orgdes        stapor
071000080911    2c                   endif
071100080911     c
071200080911    2c                   if        wdet='A'
071300080911     c                   write     or49t1c
071400080911     c                   eval      *in91 = *off
071500080911    2c                   endif
071600080911    2c                   if        wdet='B'
071700080911     c                   write     or49t1b
071800080911     c                   eval      *in92 = *off
071900080911    2c                   endif
072000080911     c
072100080911    1c                   endif
072200080911     c
072300080911     c* giro data orm
072400080911     c                   clear                   wlbdat
072500080911     c                   move      ormdao        g02inv
072600080911     c                   movel     '3'           G02err
072700080911     c                   call      'XSRDA8'
072800080911     c                   parm                    wlbdat
072900080911     c                   movel     g02dat        Stadao
073000080911     c*
073100080911     c                   movel     ormrsr        starsr
073200080912     c                   movel     ormrsr        st2rsr
073300080911     c                   movel     orminr        stainr
073400080912     c                   movel     orminr        st2inr
073500080911     c                   movel     ormlor        stalor
073600080911     C*
073700080911    1C                   IF        ormNAR=*BLANKS
073800080911     C                   EVAL      %SUBST(STALOR:21:3)=' '+ORMPRR
073900080911     c                   else
074000080911     C                   EVAL      %SUBST(STALOR:20:4)=' '+ORMnar
074100080911    1C                   endif
074200080911     c
074300080911    1c                   if        wdet='A'
074400080911     c                   write     or49d1c
074500080911    1c                   endif
074600080911    1c                   if        wdet='B'
074700080911     c                   write     or49d1b
074800080911    1c                   endif
074900080911     c
075000010614     c                   endsr
075100080912      *--------------------------------------------------------------*
075200080912      * Stmapa totale Generale
075300080912      *--------------------------------------------------------------*
075400080912     c     TotaleGEN     BEGSR
075500080923     c* Solo se c'e' e se non ho richiesto una filile e una data
075600080912     c
075700080912     c                   if        genA>0 and (i48poel=0  or skdate(2)>0)
075800080912     c                   z-add     genA          totA
075900080912     c                   z-add     genA1         totA1
076000080912     c                   z-add     genB          totB
076100080912     c                   z-add     genB1         totB1
076200080912     c                   z-add     genC          totC
076300080912     c                   z-add     genC1         totC1
076400080917     c* Calcola percentuali
076500080917     c                   exsr      CalcolaPERC
076600080912      * salto pagina
076700080912    1c                   if        *in90 = *on
076800080912     c                   write     or49t1
076900080912     c                   write     or49t2
077000080912     c                   write     or49t3
077100080912     c                   write     or49t4
077200080912     c                   eval      *in90 = *off
077300080912    1c                   endif
077400080912     c
077500080912     c*
077600080923     c  n16              write     or49d2
077700080912     c                   write     or49d1
077800080923     c  n16              write     or49d2
077900080912     c                   setoff                                       10
078000080912     c                   endif
078100080912     c
078200080912     c                   ENDSR
078300080917      *--------------------------------------------------------------*
078400080917      * calcola percentuali dei totale
078500080917      *--------------------------------------------------------------*
078600080917     c     CalcolaPERC   BEGSR
078700080917     c                   clear                   percA1
078800080917     c                   clear                   percB1
078900080917     c                   clear                   percC1
079000080917     c                   if        totA>0
079100080917     c                   eval(H)   perca1=(tota1*100)/totA
079200080917     c                   endif
079300080917     c                   if        totB>0
079400080917     c                   eval(H)   percb1=(totb1*100)/totB
079500080917     c                   endif
079600080917     c                   if        totC>0
079700080917     c                   eval(H)   percc1=(totc1*100)/totC
079800080917     c                   endif
079900080917     c                   ENDSR
080000010604      *--------------------------------------------------------------*
080100010604      * ROUTINE INIZIALE
080200010604      *--------------------------------------------------------------*
080300010604     c     *inzsr        begsr
080400010604      *
080500010604     c     *entry        plist
080600010604     c                   parm                    kpjba
080700010621
080800080908     c                   movel     kpjbu         fior48ds
080900010621
081000080908     c                   z-add     1             codut             1 0
081100080908      *
081200080908     c     *dtaara       define    §azute        azuteds
081300080908     c     *dtaara       define    §datiute      ddatiute
081400080908     c                   in(E)     *dtaara
081500080908    1c                   If        %error  or RSUT = *blanks
081600080908     c                   Clear                   Tibs34ds
081700080908     c                   Call      'TIBS34R'
081800080908     c                   Parm                    Tibs34ds
081900080908     c                   In        *dtaara
082000080908    1c                   EndIf
082100080908
082200080908     c                   Clear                   wabi
082300080908     c                   Clear                   dLat
082400080908
082500080908      * Verifica errori e autorità profilo
082600080908s   1c                   Select
082700080908      * se ho errori nei dati utente esco dal pgm
082800080908w   1c                   When      DutErr = 'E'
082900080908     c                   clear                   wabi
083000080908      * se non c'è l'abilitazione
083100080908      * --> se 1° livello, abilitazioni al terminal
083200080908      *     se 2° livello, abilitazioni al punto operativo
083300080908      *     se sede è impossibile ma se capita mando a fine il pgm
083400080908w   1c                   When      UteAut = *Blanks
083500080908if  2c                   If        DutLpo = '1'
083600080908     c                   Eval      wabi   = 'TP'
083700080908e   2c                   EndIf
083800080908if  2c                   If        DutLpo = '2'
083900080908     c                   Eval      wabi   = 'PO'
084000080908e   2c                   EndIf
084100080908if  2c                   If        DutLpo = 'S'
084200080908     c                   eval      duterr='E'
084300080908     c                   clear                   wabi
084400080908e   2c                   EndIf
084500080908      * carica le abilitazioni del profilo
084600080908x   1c                   Other
084700080908     c                   Eval      wabi = UteAut
084800080908e   1c                   EndSl
084900080908
085000080908      * controllo se ok l'abilitazione dell'utente
085100080908     c                   Clear                   Tibs02ds
085200080908     c                   Eval      T02Mod = 'C'
085300080908     c                   Eval      T02Sif = knsif
085400080908     c                   Eval      T02Cod = 'LAT'
085500080908     c                   Movel(p)  wabi          T02Ke1
085600080908     c                   Call      'TIBS02R'
085700080908     c                   Parm                    kpjba
085800080908     c                   Parm                    Tibs02ds
085900080908if  1c                   If        T02Err = *Blanks
086000080908     c                   Eval      dLat = T02Uni
086100080908e   1c                   EndIf
086200080908      * errore o non abilitato
086300080908if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
086400080908   x1c                   Else
086500080908      * Reperimento dei P.O. gestibili dall'utente
086600080908     c                   clear                   TRUL31DS
086700080908     c                   clear                   TRUL31DS2
086800080908     c                   eval      I31abi = wabi
086900080908     c                   eval      I31cdi = DUTdis
087000080908     c                   eval      I31car = DUTare
087100080908     c                   eval      I31cpo = DUTpou
087200080908     c                   call      'TRUL31R'
087300080908     c                   parm                    KPJBA
087400080908     c                   parm                    TRUL31DS
087500080908     c                   parm                    TRUL31DS2
087600080908if  2c                   if        O31pog > *zeros
087700080908     c                   movea     O31pog        $pog
087800080908     c                   movea     O31arg        $arg
087900080908     c                   movea     O31dig        $dig
088000080908     c                   else
088100080908     c                   clear                   $pog
088200080911    2c                   endif
088300080911    1c                   endif
088400080908
088500010604      * reperisco data e ora
088600010604     c                   time                    w0140
088700010604      * udate in GGMMAAAA
088800010604     c                   move      w0140         wdate
088900010604     c                   movel     w0140         wora
089000010608
089100010608      * pulisco le sk
089200010608     c                   clear                   kfile
089300030916     c                   clear                   kfile1
089400010608     c                   clear                   rfile
089500010608     c                   clear                   skdate
089600010608     c                   clear                   skpoel
089700130121     c********           Clear                   skosr
089800010621
089900010621      * controllo se devo stampare per data o per p.o.
090000080910      * 13 off - stampo data in testata e dettaglio per p.o.
090100080910      * 13 on  - stampo p.o. in testata e dettaglio per data
090200080911     c                   if        i48poel = 0   and  (i48dtal=0 or
090300080910     c                             i48dtda =  i48dtal)
090400080910     c                   eval      *in13 = *off
090500010621     c                   else
090600080910     c                   eval      *in13 = *on
090700080923     c
090800080923     c* Se stampa sintetica per tante date stampo in testata
090900080923     c                   if        i48tip='S' AND i48poel=0 and i48dtal>0
091000080923     c                             and i48dtda<>i48dtal
091100080923     c                   eval      *in13 = *off
091200010621     c                   endif
091300080923     c
091400080923     c                   endif
091500010608
091600010608      * carico la sk delle date da elaborare
091700010703     c                   eval      xd = 1
091800080911     c                   eval      skdate(xd) = i48dtda
091900080911     c                   if        i48dtda <> i48dtal
092000080911     c                   eval      wdtgio = i48dtda
092100080911     c     *iso          movel     i48dtda       wdtda
092200080911     c                   dow       wdtgio < i48dtal
092300010703     c                   eval      xd = xd +1
092400010703     c                   adddur    1:*d          wdtda
092500010703     c                   move      wdtda         wdtgio
092600010703     c                   eval      skdate(xd) = wdtgio
092700010703     c                   enddo
092800010703     c                   endif
092900020807
093000020808     c                   z-add     1             xa
093100010608
093200080909      * Elaboro tutti i p.o. A CUI L'UTENTE è abilitato
093300080911 b1  c                   if        i48poel = 0
093400080909     c                   dow       $pog(Xa)>*zeros
093500080909     c                   movel     $pog(Xa)      skpoel(Xa)
093600080909     c                   add       1             xa
093700080909     c                   enddo
093800020807 e1  c                   EndIf
093900010605      * lanciato un solo po carico questo nella sk
094000080911     c                   if        i48poel  > 0
094100080909     c                   eval      skpoel(1) = i48poel
094200010605     c                   endif
094300031002
094400031002      * carico la sk delle serie da non elaborare in statistica
094500130121     c*******            Eval      xc = 1
094600130121     c*******            Eval      TbeCod = 'OSR'
094700130121     c*****TbeCod        Setll     Tntbe01l
094800130121Do  1c*******            Do        *Hival
094900130121     c*****TbeCod        Reade     Tntbe01l
095000130121     c*******            If        %Eof(Tntbe01l)
095100130121     c*******            Leave
095200130121     c*******            EndIf
095300130121     c*******            Movel     TbeUni        dOsr
095400130121     c*******            If        d§OsrSta = *Blanks
095500130121     c*******            Iter
095600130121     c*******            EndIf
095700130121     c*******            Movel     TbeKe1        SkOsr(xc)
095800130121     c*******            Add       1             Xc
095900130121    1c*******            EndDo
096000080910     c
096100080911     c                   setoff                                       10
096200010620
096300010705      * se richiesta la stampa di controllo apro il file di stampa
096400080909     c                   if        i48DETA1= 'S'
096500080909     c                   open      fior49p1
096600010620     c                   endif
096700080911      * se richiesta la stampa di controllo apro il file di stampa
096800080911     c                   if        i48DETB1= 'S'
096900080911     c                   open      fior49p2
097000080911     c                   endif
097100080911     c
097200080911     c                   eval      *in90=*on
097300080911     c                   eval      *in91=*on
097400080911     c                   eval      *in92=*on
097500010605
097600010621      * se richiesta la creazione del file lo apro
097700080911     c***                if        i48tpel = 'F'
097800080909     c***                open      wfror01l
097900080909     c***                endif
098000010621
098100010605      * chiavi
098200080910     c     korg          klist
098300080910     c                   kfld                    ormpoe
098400080910     c                   kfld                    ormnsr
098500080910     c                   kfld                    ormnor
098600080910     c                   kfld                    ormnrv
098700010606
098800010620     c     ktab          klist
098900010620     c                   kfld                    codut
099000010620     c                   kfld                    tblcod
099100010620     c                   kfld                    tblkey
099200010620
099300010611     c     ktntbe        klist
099400010611     c                   kfld                    tbecod
099500010611     c                   kfld                    tbeke1
099600080924
099700080924     c     kroa          klist
099800080924     c                   kfld                    kroapoe
099900080924     c                   kfld                    kroadta
100000010606
100100010604     c                   endsr
