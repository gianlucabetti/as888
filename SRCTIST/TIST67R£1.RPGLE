000100110620     /*END
000200000606     H DECEDIT('0,') DATEDIT(*DMY.)
000300090928     H* TIST67R *----------------------------------------------------*
000400091007     H* Analisi clienti D-T-A-B - Filtro                             *
000500000000     H*--------------------------------------------------------------*
000600110620     ftabel00f  if   e           k disk
000700090928     FTIST67D   CF   E             WORKSTN
000800941223     D*
000900900517     D KPJBA         E DS
001000051114     D*-------------------
001100051114     D* DS PARAMETRI DI LANCIO PGM
001200051114     D*-------------------
001300090928     d tist68ds      e DS
001400090928
001500090928     d Tibs34ds      e ds
001600090928     d Tibs02ds      e ds
001700090928     d Trul31ds      e ds
001800101110     d Trul31ds2     e ds
001900090928
002000090928     d Azuteds       e ds                  Extname(Azute00f)
002100090928     d dDatiute      e ds
002200090928     d dute01        e ds
002300090928     d dlat          e ds
002400090928     d ds$3          e ds
002500090928     d ds01          e ds
002600090928
002700090928     d wlbdat          ds
002800090928     d  g02dat                 1      8  0
002900090928     d  g02inv                 9     16  0
003000090928     d  g02err                17     17
003100090928     d  g02tgi                18     22  0
003200110805     d*
003300091201     D WDAT8           DS
003400091201     D  DATADA                 1      8  0
003500091201     D  DATAA                  9     16  0
003600091201     D  GGL                   17     21  0
003700090929     d*
003800090929     d vidclv          ds
003900090929     d  vidclv1
004000090929     d  vidclv2
004100090929     d  vidclv3
004200090929     d  vidclv4
004300090929     d  clv                           1    dim(4) overlay(vidclv)
004400051114     d
004500090928     D comdal          S                   LIKE(viddal) inz
004600090928     D comal           S                   LIKE(viddal) inz
004700090928     D comrda          S                   LIKE(viddal) inz
004800090928     D comral          S                   LIKE(viddal) inz
004900090928     d dataisoda       s               d   datfmt(*iso)
005000090928     d dataisoa        s               d   datfmt(*iso)
005100090928     d num_days        s             15p 0
005200090928     d wabi            s                   like(UteAut)
005300090928     D kkut            S                   LIKE(tblkut) inz(1)
005400090928     D kcod            S                   LIKE(tblcod)
005500090928     D kkey            S                   LIKE(tblkey)
005600090929     D comage          S                   LIKE(§01rgf)
005700090928     d w003a           s              3
005800101110     D §KUT            s                   like(kkut)
005900101110     D §COD            s                   like(TBLCOD)
006000101110     D §KEY            s                   like(TBLkey)
006100101110     D §des            s             25
006200090928      *   S C H I E R E
006300110621     d msg             s             79    dim(17) ctdata perrcd(1)
006400090928     d skpog           s              3    dim(250) inz(*Zeros)
006500101110     D skARG           s              3    dim(50)
006600000000     C*---------------------------------------------------------------*
006700920925     C*
006800090928     c                   exsr      sr_inzd01
006900000615     C*
007000900509     C     FOR01         TAG
007100090928     C                   EXFMT     st67D01
007200090928     c                   Eval      *In28 = *Off
007300090928     c                   Eval      *In90 = *Off
007400090928     c                   Clear                   vidmsg
007500941223     C*
007600941223     C** CMD3 - FINE LAVORO
007700090928     C                   if        *inkc or *in08
007800090928     c                   goto      fine
007900090928     c                   endif
008000900524     C* CONTROLLO
008100900524     C                   EXSR      CONTR
008200941223     C  NKF
008300900524     COR 90              GOTO      FOR01
008400101110     C** F6 - ELABORAZIONE
008500090928     c                   clear                   tist68ds
008600101110     c                   if        vidare>*zeros
008700101110     c                   movel     vidare        st68car
008800101110     c                   endif
008900090928     C                   Z-ADD     comDal        st68dal
009000090928     C                   Z-ADD     comal         st68al
009100090928     C                   Z-ADD     comrda        st68rda
009200090928     C                   Z-ADD     comral        st68ral
009300090929     c                   z-add     comage        st68age
009400090928     c                   movel     vidclv        st68clv
009500091005     c                   if        vidsgn='-'
009600091005     c  n09              z-sub     vidsco        st68sco
009700091005     c                   else
009800091005     c  n09              z-add     vidsco        st68sco
009900091005     c                   endif
010000090928     c  n09              movel     vidrif        st68rif
010100090928     c  n09              movel     viddet        st68det
010200090928     c   09              movel     'S'           st68crt
010300110805     c                   movel     vidabl        st68abl
010400000606     C                   CLEAR                   KPJBU
010500090928     C                   MOVEL     tist68ds      KPJBU
010600110621     c                   if        %subst( knsif : 1 : 3 ) = 'FIL'
010700090928     C                   MOVEL     'ST68'        KCOAZ
010800110620     c                   else
010900110620     c                   eval      kcoaz = 'ST6H'
011000110719     c                   call      'BCH09'
011100110719     c                   parm                    KPJBA
011200110620     c                   endif
011300900523     C                   CALL      'BCH10'
011400900523     C                   PARM                    KPJBA
011500920925     C*
011600920925     C     FINE          TAG
011700000000     C                   SETON                                        LR
011800090928     c*------------------------------------------------------------------------
011900090928     c     *inzsr        begsr
012000090928     C     *ENTRY        PLIST
012100090928     C                   PARM                    KPJBA
012200090928     c     *dtaara       define    §azute        azuteds
012300090928     c     *dtaara       define    §datiute      ddatiute
012400090928     c                   in(E)     *dtaara
012500090928     c                   If        %error  or RSUT = *blanks
012600090928     c                   CLEAR                   tibs34ds
012700090928     c                   CALL      'TIBS34R'
012800090928     c                   PARM                    tibs34ds
012900090928     c                   in        *dtaara
013000090928     c                   EndIf
013100091007     c*
013200091007      * Controllo se sono in sede
013300091007     c                   Eval      *In09 = (simfel = *zeros)
013400090928     c*
013500090928     c                   Clear                   wabi
013600090928     c                   Clear                   dLat
013700090928      * Verifica errori e autorità profilo
013800090928s   1c                   Select
013900090928      * se ho errori nei dati utente esco dal pgm
014000090928w   1c                   When      duterr = 'E'
014100090928     c                   Eval      *In08 = *On
014200090928     c                   Eval      *In28 = *On
014300090928     c                   Eval      vidmsg = msg(01)
014400090928     c                   Leavesr
014500090928      * se non c'è l'abilitazione
014600090928      * --> se 1° livello, abilitazioni al terminal
014700090928      *     se 2° livello, abilitazioni al punto operativo
014800090928w   1c                   When      uteaut = *Blanks
014900090928if  2c                   If        dutlpo = '1'
015000090928     c                   Eval      wabi   = 'TP'
015100090928e   2c                   EndIf
015200090928if  2c                   If        dutlpo = '2'
015300090928     c                   Eval      wabi   = 'PO'
015400090928e   2c                   EndIf
015500090928if  2c                   If        dutlpo = 'S'
015600090928     c                   Eval      wabi   = 'AZ'
015700090928e   2c                   EndIf
015800090928      * carica le abilitazioni del profilo
015900090928x   1c                   Other
016000090928     c                   Movel     utefaf        dute01
016100090928if  2c                   If        §utegtc <> *Blanks
016200090928     c                   Eval      wabi = §utegtc
016300090928   x2c                   Else
016400090928     c                   Eval      wabi = uteaut
016500090928e   2c                   EndIf
016600090928e   1c                   EndSl
016700091006     c* Errore se utente non abilitato al "Monitoraggio Spedizioni"
016800091006     c                   if        §utemntsp<>'S'
016900091006     c                   Eval      *In08 = *On
017000091006     c                   Eval      *In28 = *On
017100091006     c                   Eval      vidmsg = msg(01)
017200091006     c                   Leavesr
017300091006     c                   endif
017400090928
017500090928      * controllo se ok l'abilitazione dell'utente
017600090928     c                   Clear                   Tibs02ds
017700090928     c                   Eval      t02mod = 'C'
017800090928     c                   Eval      t02sif = knsif
017900090928     c                   Eval      t02cod = 'LAT'
018000090928     c                   Movel(p)  wabi          t02ke1
018100090928     c                   Call      'TIBS02R'
018200090928     c                   Parm                    kpjba
018300090928     c                   Parm                    Tibs02ds
018400090928     c                   If        t02err = *Blanks
018500090928     c                   Eval      dLat = t02uni
018600090928     c                   EndIf
018700090928      * errore o non abilitato
018800090928     c                   If        t02err <> *Blanks or §latabi = 'S'
018900090928     c                   Eval      *In08 = *On
019000090928     c                   Eval      *In28 = *On
019100090928     c                   Eval      vidmsg = msg(01)
019200090928     c                   Leavesr
019300090928     c                   EndIf
019400090928
019500090928      * Reperimento dei P.O. gestibili dall'utente per i codici clienti
019600090928     c                   Clear                   Trul31ds
019700090928     c                   Eval      i31abi = wabi
019800090928     c                   Eval      i31cdi = dutdis
019900090928     c                   Eval      i31car = dutare
020000090928     c                   Eval      i31cpo = dutpou
020100090928     c                   Call      'TRUL31R'
020200090928     c                   Parm                    kpjba
020300090928     c                   Parm                    Trul31ds
020400101110     C                   parm                    TRUL31DS2
020500090928     c                   If        o31pog > *zeros
020600090928     c                   Movea     o31pog        skpog
020700101110     c                   movea     O31arg        skarg
020800090928     c                   Else
020900090928     c                   Eval      *In08 = *On
021000090928     c                   Eval      *In28 = *On
021100090928     c                   Eval      vidmsg = msg(01)
021200090928     c                   Leavesr
021300090928     c                   EndIf
021400101110     c
021500101110     c* Se utente abilitato ad almeno 2 aree, abilito la richiesta dell' area
021600101110     c                   if        skarg(2)>*zeros
021700101110     c                   seton                                        15
021800101110     c                   endif
021900090928     c*
022000090928     c* reperimento data ultima pulizia titasp
022100090928     c                   clear                   ds$3
022200090928     C                   MOVEL     '$3'          KCOD
022300090928     C                   MOVEL     '1       '    KKEY
022400090928     c     ktab          chain     tabel00f
022500090928     c                   if        %found(tabel00f)
022600090928     c                   movel     tbluni        ds$3
022700090928     c                   endif
022800110831     c* se profilo EDP vedo anche parzilizzazione per cliente bloccato
022900110831     c                   if        %subst(knmus:1:3)='EDP'
023000110831     c                   seton                                        11
023100110831     c                   endif
023200090928     C*  KLIST
023300090928     C     KTAB          KLIST
023400090928     C                   KFLD                    KKUT
023500090928     C                   KFLD                    KCOD
023600090928     C                   KFLD                    KKEY
023700090928     c                   endsr
023800090928     C*
023900090928     C*--- Inizializzazione campi del video --------------------------*
024000090928     C     sr_inzd01     BEGSR
024100090928     c                   clear                   viddal
024200090928     c                   clear                   vidal
024300090928     c                   clear                   vidrda
024400090928     c                   clear                   vidral
024500090928     c                   clear                   vidage
024600090928     c                   clear                   vidaged
024700101110     c                   clear                   vidare
024800101110     c                   clear                   vidared
024900090929     c                   eval      vidclv='DTAB'
025000090928     c                   clear                   vidsco
025100091005     c                   eval      vidsgn='-'
025200090928     c                   clear                   vidrif
025300090928     c                   eval      viddet='N'
025400110805     c                   clear                   vidabl
025500090928     c                   endsr
025600900524     C*
025700900524     C*--- CONTROLLI FORMATO1 ----------------------------------------*
025800900524     C     CONTR         BEGSR
025900091001     C                   SETOFF                                       404142
026000091001     C                   SETOFF                                       434445
026100101110     C                   SETOFF                                       4647
026200090928     C* DATA elaborazione obbligatoria
026300090928     c                   clear                   wlbdat
026400090928     C                   MOVE      VIDDal        G02DAT
026500920925     C                   MOVEL     *BLANK        G02ERR
026600941223     C                   CALL      'XSRDA8'
026700920925     C                   PARM                    WLBDAT
026800920925     C     G02ERR        IFEQ      '1'
026900090928     C                   SETON                                        402890
027000090928     c                   eval      vidmsg=msg(02)
027100920925     C                   GOTO      ENDCTR
027200920925     C                   END
027300090928     C                   Z-ADD     G02INV        comdal
027400941223     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
027500090928     C                   Z-ADD     G02DAT        VIDDal
027600900524     C*
027700090928    1c                   if        vidal>0
027800090928     c                   clear                   wlbdat
027900090928     C                   MOVE      VIDal         G02DAT
028000051114     C                   MOVEL     *BLANK        G02ERR
028100051114     C                   CALL      'XSRDA8'
028200051114     C                   PARM                    WLBDAT
028300051114     C     G02ERR        IFEQ      '1'
028400090928     C                   SETON                                        412890
028500090928     c                   eval      vidmsg=msg(02)
028600051114     C                   GOTO      ENDCTR
028700051114     C                   END
028800090928     C                   Z-ADD     G02INV        comal
028900051114     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
029000090928     C                   Z-ADD     G02DAT        VIDal
029100090928     c                   else
029200090928     c                   z-add     comdal        comal
029300090928    1c                   endif
029400941223     C*
029500900524     C* DATA DAL NO MAGGIORE DATA AL
029600090928     C     comdal        IFGT      comal
029700090928     C                   SETON                                        409028
029800090928     c                   eval      vidmsg= msg(03)
029900900524     C                   GOTO      ENDCTR
030000900524     C                   END
030100090928     c* il range tra le due date non deve superare 7gg
030200110616     c* 35 giorni se lanciato da sede
030300090928     c                   eval      dataisoda=%date(comdal:*iso)
030400090928     c                   eval      dataisoa=%date(comal:*iso)
030500090928     c                   eval      num_days=%diff(dataisoa: dataisoda: *days)
030600090928     c                   eval      num_days+=1
030700110616     c                   if        (num_days>7 and *in09 = *off) or
030800110616     c                             (num_days>35 and *in09 = *on)
030900090928     C                   SETON                                        409028
031000090928     c                   eval      vidmsg= msg(04)
031100110616     c  n09              eval      vidmsg=%trim(vidmsg) + ' sette giorni'
031200110616     c   09              eval      vidmsg=%trim(vidmsg) + ' trentacinque giorni'
031300090928     c                   goto      endctr
031400090928     c                   endif
031500091201     c* il range tra le due date deve essere di minimo 1 giorno lavorativo
031600091201     c                   clear                   wdat8
031700091201     c                   z-add     comdal        datada
031800091201     c                   z-add     comal         dataa
031900091201     c                   call      'XSRLAV8'
032000091201     c                   parm                    wdat8
032100091201     c                   if        ggl=0
032200091201     C                   SETON                                        409028
032300091201     c                   eval      vidmsg= msg(13)
032400091201     c                   goto      endctr
032500091201     c                   endif
032600090928     c* errore se superati i giorni di pulizia di titsp per il periodo richiesto
032700090928     c                   if        comdal<=§$3dp0
032800090928     C                   SETON                                        409028
032900090928     c                   eval      vidmsg= msg(06)
033000090928     c                   goto      endctr
033100090928     c                   endif
033200090928
033300090928     C* DATA riferimento obbligatoria
033400090928     c                   clear                   wlbdat
033500090928     C                   MOVE      VIDrda        G02DAT
033600090928     C                   MOVEL     *BLANK        G02ERR
033700090928     C                   CALL      'XSRDA8'
033800090928     C                   PARM                    WLBDAT
033900090928     C     G02ERR        IFEQ      '1'
034000090928     C                   SETON                                        422890
034100090928     c                   eval      vidmsg=msg(02)
034200090928     C                   GOTO      ENDCTR
034300090928     C                   END
034400090928     C                   Z-ADD     G02INV        comrda
034500090928     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
034600090928     C                   Z-ADD     G02DAT        VIDrda
034700090928     C*
034800090928    1c                   if        vidral>0
034900090928     c                   clear                   wlbdat
035000090928     C                   MOVE      VIDral        G02DAT
035100090928     C                   MOVEL     *BLANK        G02ERR
035200090928     C                   CALL      'XSRDA8'
035300090928     C                   PARM                    WLBDAT
035400090928     C     G02ERR        IFEQ      '1'
035500090928     C                   SETON                                        432890
035600090928     c                   eval      vidmsg=msg(02)
035700090928     C                   GOTO      ENDCTR
035800090928     C                   END
035900090928     C                   Z-ADD     G02INV        comral
036000090928     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
036100090928     C                   Z-ADD     G02DAT        VIDral
036200090928     c                   else
036300090928     c                   z-add     comrda        comral
036400090928    1c                   endif
036500090928     C*
036600090928     C* DATA DAL NO MAGGIORE DATA AL
036700090928     C     comrda        IFGT      comral
036800090928     C                   SETON                                        429028
036900090928     c                   eval      vidmsg= msg(03)
037000090928     C                   GOTO      ENDCTR
037100090928     C                   END
037200110616     c* il range tra le due date non deve superare 35 gg
037300090928     c                   eval      dataisoda=%date(comrda:*iso)
037400090928     c                   eval      dataisoa=%date(comral:*iso)
037500090928     c                   eval      num_days=%diff(dataisoa: dataisoda: *days)
037600090928     c                   eval      num_days+=1
037700090928     c                   if        num_days>35
037800090928     C                   SETON                                        429028
037900090928     c                   eval      vidmsg= msg(05)
038000090928     c                   goto      endctr
038100090928     c                   endif
038200091201     c* il range tra le due date deve essere di minimo 1 giorno lavorativo
038300091201     c                   clear                   wdat8
038400091201     c                   z-add     comrda        datada
038500091201     c                   z-add     comral        dataa
038600091201     c                   call      'XSRLAV8'
038700091201     c                   parm                    wdat8
038800091201     c                   if        ggl=0
038900091201     C                   SETON                                        429028
039000091201     c                   eval      vidmsg= msg(13)
039100091201     c                   goto      endctr
039200091201     c                   endif
039300090928     c* errore se superati i giorni di pulizia di titsp per il periodo richiesto
039400090928     c                   if        comrda<=§$3dp0
039500090928     C                   SETON                                        429028
039600090928     c                   eval      vidmsg= msg(06)
039700090928     c                   goto      endctr
039800090928     c                   endif
039900101110     c
040000101110     c* Commerciale e area in alternativa
040100101110     c                   if        vidare<>*blanks and vidare<>*zeros and
040200101110     c                             vidage<>*blanks and vidage<>*zeros
040300101110     c                   seton                                        442890
040400101110     c                   eval      vidmsg=msg(14)
040500101110     c                   goto      endctr
040600101110     c                   endif
040700110411     c
040800110411     c* Se non immesso il commerciale, area obbligatoria se utente abilitato
040900110411     c*    all'azienda
041000110411     c                   if        wabi='AZ' and dutpou<>046
041100110411     c                   if        (vidage=*blanks or  vidage=*zeros) and
041200110411     c                             (vidare=*blanks or  vidare=*zeros)
041300110411     c                   seton                                        472890
041400110411     c                   eval      vidmsg=msg(17)
041500110411     c                   goto      endctr
041600110411     c                   endif
041700110411     c                   endif
041800101110     c* AREA
041900101110     c* interrogazione (?)
042000101110     c                   if        %scan('?':vidare)>0
042100101110     c                   movel     kkut          §kut
042200101110     c                   movel     '05'          §cod
042300101110     c                   clear                   §key
042400101110     c                   call      'X§TABER'
042500101110     c                   parm                    §kut
042600101110     c                   parm                    §cod
042700101110     c                   parm                    §key
042800101110     c                   parm                    §des
042900101110     c                   movel     §key          vidare
043000101110     c                   movel     §des          vidared
043100101110     C                   EVAL      *IN90=*ON
043200101110     C                   EVAL      *IN47=*ON
043300101110     C                   GOTO      ENDctr
043400101110     c                   endif
043500101110     c* Controllo in tabella
043600101110     c                   clear                   vidared
043700101110     c                   if        vidare<>*blanks and vidare<>*zeros
043800101110     c                   movel     '05'          kcod
043900101110     c                   movel(p)  vidare        kkey
044000101110     c     ktab          chain     tabel                              30
044100101110    2c                   if        *in30 OR tblflg<>' '
044200101110     c                   seton                                        472890
044300101110     c                   eval      vidmsg=msg(15)
044400101110     c                   goto      endctr
044500101110     c                   endif
044600101110     c                   movel     tbluni        vidared
044700101110     c* Controllo se utente abilitato
044800101110     c     vidare        lookup    skarg                                  30
044900101110    2c                   if        not *in30
045000101110     c                   seton                                        472890
045100101110     c                   eval      vidmsg=msg(16)
045200101110     c                   goto      endctr
045300101110     c                   endif
045400101110     c                   endif
045500101110     c
045600101110     c
045700090928     c* COMMERCIALE UNIFICANTE
045800090929     c* interrogazione (?)
045900090929     c                   if        %scan('?':vidage)>0
046000090929     C                   clear                   DES              25
046100090929     C                   clear                   KKEY
046200090929     C                   CALL      'TRTB85R'
046300090929     C                   PARM                    KPJBA
046400090929     C                   PARM                    PO                3
046500090929     C                   PARM                    KKEY
046600090929     C                   PARM                    DES
046700090929     C                   PARM      'S'           p_solounif        1
046800090929     C                   MOVEL     kkey          vidage
046900090929     C                   MOVEL     DES           vidaged
047000090929     C                   EVAL      *IN90=*ON
047100090929     C                   GOTO      ENDctr
047200090929     c                   endif
047300090929     c* obbligatorio se utente non ha l'autorizzazione "AZ", "AO", "RA"
047400090928     c                   clear                   vidaged
047401111024     c                   clear                   comage
047500101115     c**                 if        uteaut<>'AZ' and
047600101115     c**                           uteaut<>'AO' and
047700101115     c**                           uteaut<>'RA' and
047800101115     c**                           (vidage=*zeros or vidage=*blanks)
047900101115     c**                 seton                                        442890
048000101115     c**                 eval      vidmsg=msg(07)
048100101115     c**                 goto      endctr
048200101115     c**                 endif
048300090929    1c                   if        vidage<>*zeros and vidage<>*blanks
048400090929     c* deve contenere numeri
048500090929     c                   eval      *in10   = *off
048600090929     c                   testn                   vidage               10
048700090929    2c                   if             *in10  = *off
048800090929     c                             or  %subst(vidage: 7: 1) < *zero
048900090929     c                   seton                                        449028
049000090929     c                   eval      vidmsg=msg(7)
049100090929     c                   goto      endctr
049200090929    2c                   endif
049300090929     c                   move      vidage        comage
049400090928     c* la filiale cliente deve essere una filiale gestita dall'utente
049500090928     c                   movel     vidage        w003a
049600090929    2c                   if        %lookup(w003a:skpog)=0
049700090928     c                   seton                                        449028
049800090928     c                   eval      vidmsg=msg(8)
049900090928     c                   goto      endctr
050000090929    2c                   endif
050100090928     c                   clear                   ds01
050200090928     c                   eval      kcod='01'
050300090929     c                   movel     comage        kkey
050400090928     c     ktab          chain     tabel00f
050500090929    2c                   if        not %found (tabel00f)
050600090928     c                   seton                                        449028
050700090928     c                   eval      vidmsg=msg(7)
050800090928     c                   goto      endctr
050900090929    2c                   endif
051000090928     c                   movel     tbluni        ds01
051100090928     c                   movel     §01age        vidaged
051200090929    2c                   if        §01rgf<>comage
051300090928     c                   seton                                        449028
051400090928     c                   eval      vidmsg=msg(9)
051500090928     c                   goto      endctr
051600090928     c
051700090929    2c                   endif
051800090929    1c                   endif
051900090929     c* categoria cliente
052000090929     c* specificarne almeno una
052100090929     c                   if        vidclv=*blanks
052200090929     c                   seton                                        459028
052300090929     c                   eval      vidmsg=msg(10)
052400090929     c                   goto      endctr
052500090929     c                   endif
052600090929     c* non ce ne devono essere di ripetute
052700090929     c                   do        3             ii                1 0
052800090929     c                   if        clv(ii)<>*blanks
052900090929     c                             and %scan(clv(ii):vidclv:(ii+1))>0
053000090929     c                   seton                                        459028
053100090929     c                   eval      vidmsg=msg(11)
053200091001     c                   eval      %subst(vidmsg:20:1)=clv(ii)
053300090929     c                   goto      endctr
053400090929     c                   endif
053500090929     c                   add       1             ii
053600090929     c                   enddo
053700091005     c* segno + , -
053800091005     c                   if        vidsgn=*blanks
053900091005     c                   seton                                        469028
054000091005     c                   eval      vidmsg=msg(12)
054100091005     c                   goto      endctr
054200091005     c                   endif
054300941223     C*
054400900524     C     ENDCTR        ENDSR
054500920925     C*
054600090928**
054700091007Utente non autorizzato all'Analisi Clienti D-T-A-B                            01
054800091001Data errata o mancante                                                        02
054900090928Data al minore di data dal                                                    03
055000110616Il periodo di analisi non deve essere superiore di                            04
055100091007Il periodo di riferimento non deve essere superiore di trentacinque giorni    05
055200090928Periodo non elaborabile per mancanza di dati                                  06
055300090928Codice Commerciale errato o mancante                                          07
055400090928Utente non abilitato al codice immesso                                        08
055500090928Il codice immesso non è quello di un commerciale unificante                   09
055600090929Selezionare almeno una categoria cliente ('D','T','A' o 'B')                  10
055700091001Categoria cliente ' ' selezionata più volte                                   11
055800091005Indicare il segno:'+' oppure '-'                                              12
055900091201Periodo richiesto errato: non presenti giorni lavorativi                      13
056000101110Immettere o l'area o il commerciale unificante                                14
056100101110Codice AREA inesistente                                                       15
056200101110Utente non abilitato all'area indicata                                        16
056300110411Obbligatorio indicare il codice AREA da elaborare                             17
