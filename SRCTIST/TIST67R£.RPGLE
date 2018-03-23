000100110620     /*END
000200000606     H DECEDIT('0,') DATEDIT(*DMY.)
000300090928     H* TIST67R *----------------------------------------------------*
000400091007     H* Analisi clienti D-T-A-B - Filtro                             *
000500000000     H*--------------------------------------------------------------*
000600110620     ftabel00f  if   e           k disk
000700130801     fAZCMM01L  if   e           k disk
000800090928     FTIST67D   CF   E             WORKSTN
000900941223     D*
001000900517     D KPJBA         E DS
001100051114     D*-------------------
001200051114     D* DS PARAMETRI DI LANCIO PGM
001300051114     D*-------------------
001400090928     d tist68ds      e DS
001500090928
001600090928     d Tibs34ds      e ds
001700090928     d Tibs02ds      e ds
001800130801     d TRMK43ds      e ds                  inz
001900090928     d Trul31ds      e ds
002000101110     d Trul31ds2     e ds
002100090928
002200090928     d Azuteds       e ds                  Extname(Azute00f)
002300090928     d dDatiute      e ds
002400090928     d dute01        e ds
002500090928     d dlat          e ds
002600090928     d ds$3          e ds
002700090928
002800090928     d wlbdat          ds
002900090928     d  g02dat                 1      8  0
003000090928     d  g02inv                 9     16  0
003100090928     d  g02err                17     17
003200090928     d  g02tgi                18     22  0
003300110805     d*
003400091201     D WDAT8           DS
003500091201     D  DATADA                 1      8  0
003600091201     D  DATAA                  9     16  0
003700091201     D  GGL                   17     21  0
003800090929     d*
003900090929     d vidclv          ds
004000090929     d  vidclv1
004100090929     d  vidclv2
004200090929     d  vidclv3
004300090929     d  vidclv4
004400090929     d  clv                           1    dim(4) overlay(vidclv)
004500051114     d
004600090928     D comdal          S                   LIKE(viddal) inz
004700090928     D comal           S                   LIKE(viddal) inz
004800090928     D comrda          S                   LIKE(viddal) inz
004900090928     D comral          S                   LIKE(viddal) inz
005000090928     d dataisoda       s               d   datfmt(*iso)
005100090928     d dataisoa        s               d   datfmt(*iso)
005200090928     d num_days        s             15p 0
005300090928     d wabi            s                   like(UteAut)
005400090928     D kkut            S                   LIKE(tblkut) inz(1)
005500090928     D kcod            S                   LIKE(tblcod)
005600090928     D kkey            S                   LIKE(tblkey)
005700130801     D comage          S                   LIKE(CMMcod)
005800090928     d w003a           s              3
005900101110     D §KUT            s                   like(kkut)
006000101110     D §COD            s                   like(TBLCOD)
006100101110     D §KEY            s                   like(TBLkey)
006200101110     D §des            s             25
006300090928      *   S C H I E R E
006400110621     d msg             s             79    dim(17) ctdata perrcd(1)
006500090928     d skpog           s              3    dim(250) inz(*Zeros)
006600101110     D skARG           s              3    dim(50)
006700000000     C*---------------------------------------------------------------*
006800920925     C*
006900090928     c                   exsr      sr_inzd01
007000000615     C*
007100900509     C     FOR01         TAG
007200090928     C                   EXFMT     st67D01
007300090928     c                   Eval      *In28 = *Off
007400090928     c                   Eval      *In90 = *Off
007500090928     c                   Clear                   vidmsg
007600941223     C*
007700941223     C** CMD3 - FINE LAVORO
007800090928     C                   if        *inkc or *in08
007900090928     c                   goto      fine
008000090928     c                   endif
008100900524     C* CONTROLLO
008200900524     C                   EXSR      CONTR
008300941223     C  NKF
008400900524     COR 90              GOTO      FOR01
008500101110     C** F6 - ELABORAZIONE
008600090928     c                   clear                   tist68ds
008700101110     c                   if        vidare>*zeros
008800101110     c                   movel     vidare        st68car
008900101110     c                   endif
009000090928     C                   Z-ADD     comDal        st68dal
009100090928     C                   Z-ADD     comal         st68al
009200090928     C                   Z-ADD     comrda        st68rda
009300090928     C                   Z-ADD     comral        st68ral
009400090929     c                   z-add     comage        st68age
009500090928     c                   movel     vidclv        st68clv
009600091005     c                   if        vidsgn='-'
009700091005     c  n09              z-sub     vidsco        st68sco
009800091005     c                   else
009900091005     c  n09              z-add     vidsco        st68sco
010000091005     c                   endif
010100090928     c  n09              movel     vidrif        st68rif
010200090928     c  n09              movel     viddet        st68det
010300090928     c   09              movel     'S'           st68crt
010400110805     c                   movel     vidabl        st68abl
010500000606     C                   CLEAR                   KPJBU
010600090928     C                   MOVEL     tist68ds      KPJBU
010700110621     c                   if        %subst( knsif : 1 : 3 ) = 'FIL'
010800090928     C                   MOVEL     'ST68'        KCOAZ
010900110620     c                   else
011000110620     c                   eval      kcoaz = 'ST6H'
011100110719     c                   call      'BCH09'
011200110719     c                   parm                    KPJBA
011300110620     c                   endif
011400900523     C                   CALL      'BCH10'
011500900523     C                   PARM                    KPJBA
011600920925     C*
011700920925     C     FINE          TAG
011800000000     C                   SETON                                        LR
011900090928     c*------------------------------------------------------------------------
012000090928     c     *inzsr        begsr
012100090928     C     *ENTRY        PLIST
012200090928     C                   PARM                    KPJBA
012300090928     c     *dtaara       define    §azute        azuteds
012400090928     c     *dtaara       define    §datiute      ddatiute
012500090928     c                   in(E)     *dtaara
012600090928     c                   If        %error  or RSUT = *blanks
012700090928     c                   CLEAR                   tibs34ds
012800090928     c                   CALL      'TIBS34R'
012900090928     c                   PARM                    tibs34ds
013000090928     c                   in        *dtaara
013100090928     c                   EndIf
013200091007     c*
013300091007      * Controllo se sono in sede
013400091007     c                   Eval      *In09 = (simfel = *zeros)
013500090928     c*
013600090928     c                   Clear                   wabi
013700090928     c                   Clear                   dLat
013800090928      * Verifica errori e autorità profilo
013900090928s   1c                   Select
014000090928      * se ho errori nei dati utente esco dal pgm
014100090928w   1c                   When      duterr = 'E'
014200090928     c                   Eval      *In08 = *On
014300090928     c                   Eval      *In28 = *On
014400090928     c                   Eval      vidmsg = msg(01)
014500090928     c                   Leavesr
014600090928      * se non c'è l'abilitazione
014700090928      * --> se 1° livello, abilitazioni al terminal
014800090928      *     se 2° livello, abilitazioni al punto operativo
014900090928w   1c                   When      uteaut = *Blanks
015000090928if  2c                   If        dutlpo = '1'
015100090928     c                   Eval      wabi   = 'TP'
015200090928e   2c                   EndIf
015300090928if  2c                   If        dutlpo = '2'
015400090928     c                   Eval      wabi   = 'PO'
015500090928e   2c                   EndIf
015600090928if  2c                   If        dutlpo = 'S'
015700090928     c                   Eval      wabi   = 'AZ'
015800090928e   2c                   EndIf
015900090928      * carica le abilitazioni del profilo
016000090928x   1c                   Other
016100090928     c                   Movel     utefaf        dute01
016200090928if  2c                   If        §utegtc <> *Blanks
016300090928     c                   Eval      wabi = §utegtc
016400090928   x2c                   Else
016500090928     c                   Eval      wabi = uteaut
016600090928e   2c                   EndIf
016700090928e   1c                   EndSl
016800091006     c* Errore se utente non abilitato al "Monitoraggio Spedizioni"
016900091006     c                   if        §utemntsp<>'S'
017000091006     c                   Eval      *In08 = *On
017100091006     c                   Eval      *In28 = *On
017200091006     c                   Eval      vidmsg = msg(01)
017300091006     c                   Leavesr
017400091006     c                   endif
017500090928
017600090928      * controllo se ok l'abilitazione dell'utente
017700090928     c                   Clear                   Tibs02ds
017800090928     c                   Eval      t02mod = 'C'
017900090928     c                   Eval      t02sif = knsif
018000090928     c                   Eval      t02cod = 'LAT'
018100090928     c                   Movel(p)  wabi          t02ke1
018200090928     c                   Call      'TIBS02R'
018300090928     c                   Parm                    kpjba
018400090928     c                   Parm                    Tibs02ds
018500090928     c                   If        t02err = *Blanks
018600090928     c                   Eval      dLat = t02uni
018700090928     c                   EndIf
018800090928      * errore o non abilitato
018900090928     c                   If        t02err <> *Blanks or §latabi = 'S'
019000090928     c                   Eval      *In08 = *On
019100090928     c                   Eval      *In28 = *On
019200090928     c                   Eval      vidmsg = msg(01)
019300090928     c                   Leavesr
019400090928     c                   EndIf
019500090928
019600090928      * Reperimento dei P.O. gestibili dall'utente per i codici clienti
019700090928     c                   Clear                   Trul31ds
019800090928     c                   Eval      i31abi = wabi
019900090928     c                   Eval      i31cdi = dutdis
020000090928     c                   Eval      i31car = dutare
020100090928     c                   Eval      i31cpo = dutpou
020200090928     c                   Call      'TRUL31R'
020300090928     c                   Parm                    kpjba
020400090928     c                   Parm                    Trul31ds
020500101110     C                   parm                    TRUL31DS2
020600090928     c                   If        o31pog > *zeros
020700090928     c                   Movea     o31pog        skpog
020800101110     c                   movea     O31arg        skarg
020900090928     c                   Else
021000090928     c                   Eval      *In08 = *On
021100090928     c                   Eval      *In28 = *On
021200090928     c                   Eval      vidmsg = msg(01)
021300090928     c                   Leavesr
021400090928     c                   EndIf
021500101110     c
021600101110     c* Se utente abilitato ad almeno 2 aree, abilito la richiesta dell' area
021700101110     c                   if        skarg(2)>*zeros
021800101110     c                   seton                                        15
021900101110     c                   endif
022000090928     c*
022100090928     c* reperimento data ultima pulizia titasp
022200090928     c                   clear                   ds$3
022300090928     C                   MOVEL     '$3'          KCOD
022400090928     C                   MOVEL     '1       '    KKEY
022500090928     c     ktab          chain     tabel00f
022600090928     c                   if        %found(tabel00f)
022700090928     c                   movel     tbluni        ds$3
022800090928     c                   endif
022900110831     c* se profilo EDP vedo anche parzilizzazione per cliente bloccato
023000110831     c                   if        %subst(knmus:1:3)='EDP'
023100110831     c                   seton                                        11
023200110831     c                   endif
023300090928     C*  KLIST
023400090928     C     KTAB          KLIST
023500090928     C                   KFLD                    KKUT
023600090928     C                   KFLD                    KCOD
023700090928     C                   KFLD                    KKEY
023800090928     c                   endsr
023900090928     C*
024000090928     C*--- Inizializzazione campi del video --------------------------*
024100090928     C     sr_inzd01     BEGSR
024200090928     c                   clear                   viddal
024300090928     c                   clear                   vidal
024400090928     c                   clear                   vidrda
024500090928     c                   clear                   vidral
024600090928     c                   clear                   vidage
024700090928     c                   clear                   vidaged
024800101110     c                   clear                   vidare
024900101110     c                   clear                   vidared
025000090929     c                   eval      vidclv='DTAB'
025100090928     c                   clear                   vidsco
025200091005     c                   eval      vidsgn='-'
025300090928     c                   clear                   vidrif
025400090928     c                   eval      viddet='N'
025500110805     c                   clear                   vidabl
025600090928     c                   endsr
025700900524     C*
025800900524     C*--- CONTROLLI FORMATO1 ----------------------------------------*
025900900524     C     CONTR         BEGSR
026000091001     C                   SETOFF                                       404142
026100091001     C                   SETOFF                                       434445
026200101110     C                   SETOFF                                       4647
026300090928     C* DATA elaborazione obbligatoria
026400090928     c                   clear                   wlbdat
026500090928     C                   MOVE      VIDDal        G02DAT
026600920925     C                   MOVEL     *BLANK        G02ERR
026700941223     C                   CALL      'XSRDA8'
026800920925     C                   PARM                    WLBDAT
026900920925     C     G02ERR        IFEQ      '1'
027000090928     C                   SETON                                        402890
027100090928     c                   eval      vidmsg=msg(02)
027200920925     C                   GOTO      ENDCTR
027300920925     C                   END
027400090928     C                   Z-ADD     G02INV        comdal
027500941223     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
027600090928     C                   Z-ADD     G02DAT        VIDDal
027700900524     C*
027800090928    1c                   if        vidal>0
027900090928     c                   clear                   wlbdat
028000090928     C                   MOVE      VIDal         G02DAT
028100051114     C                   MOVEL     *BLANK        G02ERR
028200051114     C                   CALL      'XSRDA8'
028300051114     C                   PARM                    WLBDAT
028400051114     C     G02ERR        IFEQ      '1'
028500090928     C                   SETON                                        412890
028600090928     c                   eval      vidmsg=msg(02)
028700051114     C                   GOTO      ENDCTR
028800051114     C                   END
028900090928     C                   Z-ADD     G02INV        comal
029000051114     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
029100090928     C                   Z-ADD     G02DAT        VIDal
029200090928     c                   else
029300090928     c                   z-add     comdal        comal
029400090928    1c                   endif
029500941223     C*
029600900524     C* DATA DAL NO MAGGIORE DATA AL
029700090928     C     comdal        IFGT      comal
029800090928     C                   SETON                                        409028
029900090928     c                   eval      vidmsg= msg(03)
030000900524     C                   GOTO      ENDCTR
030100900524     C                   END
030200090928     c* il range tra le due date non deve superare 7gg
030300110616     c* 35 giorni se lanciato da sede
030400090928     c                   eval      dataisoda=%date(comdal:*iso)
030500090928     c                   eval      dataisoa=%date(comal:*iso)
030600090928     c                   eval      num_days=%diff(dataisoa: dataisoda: *days)
030700090928     c                   eval      num_days+=1
030800110616     c                   if        (num_days>7 and *in09 = *off) or
030900110616     c                             (num_days>35 and *in09 = *on)
031000090928     C                   SETON                                        409028
031100090928     c                   eval      vidmsg= msg(04)
031200110616     c  n09              eval      vidmsg=%trim(vidmsg) + ' sette giorni'
031300110616     c   09              eval      vidmsg=%trim(vidmsg) + ' trentacinque giorni'
031400090928     c                   goto      endctr
031500090928     c                   endif
031600091201     c* il range tra le due date deve essere di minimo 1 giorno lavorativo
031700091201     c                   clear                   wdat8
031800091201     c                   z-add     comdal        datada
031900091201     c                   z-add     comal         dataa
032000091201     c                   call      'XSRLAV8'
032100091201     c                   parm                    wdat8
032200091201     c                   if        ggl=0
032300091201     C                   SETON                                        409028
032400091201     c                   eval      vidmsg= msg(13)
032500091201     c                   goto      endctr
032600091201     c                   endif
032700090928     c* errore se superati i giorni di pulizia di titsp per il periodo richiesto
032800090928     c                   if        comdal<=§$3dp0
032900090928     C                   SETON                                        409028
033000090928     c                   eval      vidmsg= msg(06)
033100090928     c                   goto      endctr
033200090928     c                   endif
033300090928
033400090928     C* DATA riferimento obbligatoria
033500090928     c                   clear                   wlbdat
033600090928     C                   MOVE      VIDrda        G02DAT
033700090928     C                   MOVEL     *BLANK        G02ERR
033800090928     C                   CALL      'XSRDA8'
033900090928     C                   PARM                    WLBDAT
034000090928     C     G02ERR        IFEQ      '1'
034100090928     C                   SETON                                        422890
034200090928     c                   eval      vidmsg=msg(02)
034300090928     C                   GOTO      ENDCTR
034400090928     C                   END
034500090928     C                   Z-ADD     G02INV        comrda
034600090928     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
034700090928     C                   Z-ADD     G02DAT        VIDrda
034800090928     C*
034900090928    1c                   if        vidral>0
035000090928     c                   clear                   wlbdat
035100090928     C                   MOVE      VIDral        G02DAT
035200090928     C                   MOVEL     *BLANK        G02ERR
035300090928     C                   CALL      'XSRDA8'
035400090928     C                   PARM                    WLBDAT
035500090928     C     G02ERR        IFEQ      '1'
035600090928     C                   SETON                                        432890
035700090928     c                   eval      vidmsg=msg(02)
035800090928     C                   GOTO      ENDCTR
035900090928     C                   END
036000090928     C                   Z-ADD     G02INV        comral
036100090928     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
036200090928     C                   Z-ADD     G02DAT        VIDral
036300090928     c                   else
036400090928     c                   z-add     comrda        comral
036500090928    1c                   endif
036600090928     C*
036700090928     C* DATA DAL NO MAGGIORE DATA AL
036800090928     C     comrda        IFGT      comral
036900090928     C                   SETON                                        429028
037000090928     c                   eval      vidmsg= msg(03)
037100090928     C                   GOTO      ENDCTR
037200090928     C                   END
037300110616     c* il range tra le due date non deve superare 35 gg
037400090928     c                   eval      dataisoda=%date(comrda:*iso)
037500090928     c                   eval      dataisoa=%date(comral:*iso)
037600090928     c                   eval      num_days=%diff(dataisoa: dataisoda: *days)
037700090928     c                   eval      num_days+=1
037800090928     c                   if        num_days>35
037900090928     C                   SETON                                        429028
038000090928     c                   eval      vidmsg= msg(05)
038100090928     c                   goto      endctr
038200090928     c                   endif
038300091201     c* il range tra le due date deve essere di minimo 1 giorno lavorativo
038400091201     c                   clear                   wdat8
038500091201     c                   z-add     comrda        datada
038600091201     c                   z-add     comral        dataa
038700091201     c                   call      'XSRLAV8'
038800091201     c                   parm                    wdat8
038900091201     c                   if        ggl=0
039000091201     C                   SETON                                        429028
039100091201     c                   eval      vidmsg= msg(13)
039200091201     c                   goto      endctr
039300091201     c                   endif
039400090928     c* errore se superati i giorni di pulizia di titsp per il periodo richiesto
039500090928     c                   if        comrda<=§$3dp0
039600090928     C                   SETON                                        429028
039700090928     c                   eval      vidmsg= msg(06)
039800090928     c                   goto      endctr
039900090928     c                   endif
040000101110     c
040100101110     c* Commerciale e area in alternativa
040200101110     c                   if        vidare<>*blanks and vidare<>*zeros and
040300101110     c                             vidage<>*blanks and vidage<>*zeros
040400101110     c                   seton                                        442890
040500101110     c                   eval      vidmsg=msg(14)
040600101110     c                   goto      endctr
040700101110     c                   endif
040800110411     c
040900110411     c* Se non immesso il commerciale, area obbligatoria se utente abilitato
041000110411     c*    all'azienda
041100110411     c                   if        wabi='AZ' and dutpou<>046
041200110411     c                   if        (vidage=*blanks or  vidage=*zeros) and
041300110411     c                             (vidare=*blanks or  vidare=*zeros)
041400110411     c                   seton                                        472890
041500110411     c                   eval      vidmsg=msg(17)
041600110411     c                   goto      endctr
041700110411     c                   endif
041800110411     c                   endif
041900101110     c* AREA
042000101110     c* interrogazione (?)
042100101110     c                   if        %scan('?':vidare)>0
042200101110     c                   movel     kkut          §kut
042300101110     c                   movel     '05'          §cod
042400101110     c                   clear                   §key
042500101110     c                   call      'X§TABER'
042600101110     c                   parm                    §kut
042700101110     c                   parm                    §cod
042800101110     c                   parm                    §key
042900101110     c                   parm                    §des
043000101110     c                   movel     §key          vidare
043100101110     c                   movel     §des          vidared
043200101110     C                   EVAL      *IN90=*ON
043300101110     C                   EVAL      *IN47=*ON
043400101110     C                   GOTO      ENDctr
043500101110     c                   endif
043600101110     c* Controllo in tabella
043700101110     c                   clear                   vidared
043800101110     c                   if        vidare<>*blanks and vidare<>*zeros
043900101110     c                   movel     '05'          kcod
044000101110     c                   movel(p)  vidare        kkey
044100101110     c     ktab          chain     tabel                              30
044200101110    2c                   if        *in30 OR tblflg<>' '
044300101110     c                   seton                                        472890
044400101110     c                   eval      vidmsg=msg(15)
044500101110     c                   goto      endctr
044600101110     c                   endif
044700101110     c                   movel     tbluni        vidared
044800101110     c* Controllo se utente abilitato
044900101110     c     vidare        lookup    skarg                                  30
045000101110    2c                   if        not *in30
045100101110     c                   seton                                        472890
045200101110     c                   eval      vidmsg=msg(16)
045300101110     c                   goto      endctr
045400101110     c                   endif
045500101110     c                   endif
045600101110     c
045700101110     c
045800090928     c* COMMERCIALE UNIFICANTE
045900090929     c* interrogazione (?)
046000090929     c                   if        %scan('?':vidage)>0
046100130801     c                   clear                   TRMK43ds
046200130801     c                   eval      iMK43solU = 'S'
046300130801     C                   CALL      'TRMK43R'
046400090929     C                   PARM                    KPJBA
046500130801     C                   PARM                    TRMK43ds
046600130801     c                   if        oMK43err = *off  and
046700130801     c                             oMK43fxx = *blank
046800130801     C                   MOVEL     oMK43cmm      vidage
046900130801     C                   MOVEL     oMK43des      vidaged
047000130801     c                   endif
047100090929     C                   EVAL      *IN90=*ON
047200090929     C                   GOTO      ENDctr
047300090929     c                   endif
047400090929     c* obbligatorio se utente non ha l'autorizzazione "AZ", "AO", "RA"
047500090928     c                   clear                   vidaged
047600111024     c                   clear                   comage
047700101115     c**                 if        uteaut<>'AZ' and
047800101115     c**                           uteaut<>'AO' and
047900101115     c**                           uteaut<>'RA' and
048000101115     c**                           (vidage=*zeros or vidage=*blanks)
048100101115     c**                 seton                                        442890
048200101115     c**                 eval      vidmsg=msg(07)
048300101115     c**                 goto      endctr
048400101115     c**                 endif
048500090929    1c                   if        vidage<>*zeros and vidage<>*blanks
048600090929     c* deve contenere numeri
048700090929     c                   eval      *in10   = *off
048800090929     c                   testn                   vidage               10
048900090929    2c                   if             *in10  = *off
049000090929     c                             or  %subst(vidage: 7: 1) < *zero
049100090929     c                   seton                                        449028
049200090929     c                   eval      vidmsg=msg(7)
049300090929     c                   goto      endctr
049400090929    2c                   endif
049500090929     c                   move      vidage        comage
049600090928     c* la filiale cliente deve essere una filiale gestita dall'utente
049700090928     c                   movel     vidage        w003a
049800090929    2c                   if        %lookup(w003a:skpog)=0
049900090928     c                   seton                                        449028
050000090928     c                   eval      vidmsg=msg(8)
050100090928     c                   goto      endctr
050200090929    2c                   endif
050300130801     c     ComAge        chain     AZCMM000
050400130801    2c                   if        not %found (AZCMM01L)
050500090928     c                   seton                                        449028
050600090928     c                   eval      vidmsg=msg(7)
050700090928     c                   goto      endctr
050800090929    2c                   endif
050900130801     c                   movel     CMMdes        vidaged
051000130801    2c                   if        CMMuni <> comage
051100090928     c                   seton                                        449028
051200090928     c                   eval      vidmsg=msg(9)
051300090928     c                   goto      endctr
051400090928     c
051500090929    2c                   endif
051600090929    1c                   endif
051700090929     c* categoria cliente
051800090929     c* specificarne almeno una
051900090929     c                   if        vidclv=*blanks
052000090929     c                   seton                                        459028
052100090929     c                   eval      vidmsg=msg(10)
052200090929     c                   goto      endctr
052300090929     c                   endif
052400090929     c* non ce ne devono essere di ripetute
052500090929     c                   do        3             ii                1 0
052600090929     c                   if        clv(ii)<>*blanks
052700090929     c                             and %scan(clv(ii):vidclv:(ii+1))>0
052800090929     c                   seton                                        459028
052900090929     c                   eval      vidmsg=msg(11)
053000091001     c                   eval      %subst(vidmsg:20:1)=clv(ii)
053100090929     c                   goto      endctr
053200090929     c                   endif
053300090929     c                   add       1             ii
053400090929     c                   enddo
053500091005     c* segno + , -
053600091005     c                   if        vidsgn=*blanks
053700091005     c                   seton                                        469028
053800091005     c                   eval      vidmsg=msg(12)
053900091005     c                   goto      endctr
054000091005     c                   endif
054100941223     C*
054200900524     C     ENDCTR        ENDSR
054300920925     C*
054400090928**
054500091007Utente non autorizzato all'Analisi Clienti D-T-A-B                            01
054600091001Data errata o mancante                                                        02
054700090928Data al minore di data dal                                                    03
054800110616Il periodo di analisi non deve essere superiore di                            04
054900091007Il periodo di riferimento non deve essere superiore di trentacinque giorni    05
055000090928Periodo non elaborabile per mancanza di dati                                  06
055100090928Codice Commerciale errato o mancante                                          07
055200090928Utente non abilitato al codice immesso                                        08
055300090928Il codice immesso non è quello di un commerciale unificante                   09
055400090929Selezionare almeno una categoria cliente ('D','T','A' o 'B')                  10
055500091001Categoria cliente ' ' selezionata più volte                                   11
055600091005Indicare il segno:'+' oppure '-'                                              12
055700091201Periodo richiesto errato: non presenti giorni lavorativi                      13
055800101110Immettere o l'area o il commerciale unificante                                14
055900101110Codice AREA inesistente                                                       15
056000101110Utente non abilitato all'area indicata                                        16
056100110411Obbligatorio indicare il codice AREA da elaborare                             17
