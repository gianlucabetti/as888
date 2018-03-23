000100110620     /*END
000200000606     H DECEDIT('0,') DATEDIT(*DMY.)
000300111020     H* TNTAE1R *----------------------------------------------------*
000400111020     H* Analisi clienti con avviso al destinatario                   *
000500000000     H*--------------------------------------------------------------*
000600110620     ftabel00f  if   e           k disk
000700111020     FTNTAE1D   CF   E             WORKSTN
000800941223     D*
000900900517     D KPJBA         E DS
001000051114     D*-------------------
001100051114     D* DS PARAMETRI DI LANCIO PGM
001200051114     D*-------------------
001300111020     d tntae2ds      e DS
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
004400051114     d
004500090928     D comdal          S                   LIKE(viddal) inz
004600090928     D comal           S                   LIKE(viddal) inz
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
007100111020     C                   EXFMT     tae1D01
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
008500111020     c                   clear                   tntae2ds
008600101110     c                   if        vidare>*zeros
008700111020     c                   movel     vidare        tae2car
008800101110     c                   endif
008900111020     C                   Z-ADD     comDal        tae2dal
009000111020     C                   Z-ADD     comal         tae2al
009300111020     c                   z-add     comage        tae2age
010200111020     c   09              movel     'S'           tae2crt
010400000606     C                   CLEAR                   KPJBU
010500111020     C                   MOVEL     tntae2ds      KPJBU
010900111020     c                   eval      kcoaz = 'TAE2'
011000110719     c                   call      'BCH09'
011100110719     c                   parm                    KPJBA
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
024500090928     c                   clear                   vidage
024600090928     c                   clear                   vidaged
024700101110     c                   clear                   vidare
024800101110     c                   clear                   vidared
025500090928     c                   endsr
025600900524     C*
025700900524     C*--- CONTROLLI FORMATO1 ----------------------------------------*
025800900524     C     CONTR         BEGSR
025900111020     C                   SETOFF                                       404144
026100111020     C                   SETOFF                                       47
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
030100111024     c* il range tra le due date non deve superare 32gg
030300090928     c                   eval      dataisoda=%date(comdal:*iso)
030400090928     c                   eval      dataisoa=%date(comal:*iso)
030500090928     c                   eval      num_days=%diff(dataisoa: dataisoda: *days)
030600090928     c                   eval      num_days+=1
030700111024     c                   if        (num_days>32)
030900090928     C                   SETON                                        409028
031000090928     c                   eval      vidmsg= msg(04)
031200111024     c                   eval      vidmsg=%trim(vidmsg) + ' trentadue giorni'
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
054300941223     C*
054400900524     C     ENDCTR        ENDSR
054500920925     C*
054600090928**
054700111020Utente non autorizzato all'Analisi                                            01
054800091001Data errata o mancante                                                        02
054900090928Data al minore di data dal                                                    03
055000111024Il periodo dal/al non  deve essere superiore di                               04
055100111020                                                                              05
055200090928Periodo non elaborabile per mancanza di dati                                  06
055300090928Codice Commerciale errato o mancante                                          07
055400090928Utente non abilitato al codice immesso                                        08
055500090928Il codice immesso non è quello di un commerciale unificante                   09
055600111020                                                                              10
055700111020                                                                              11
055800111020                                                                              12
055900091201Periodo richiesto errato: non presenti giorni lavorativi                      13
056000101110Immettere o l'area o il commerciale unificante                                14
056100101110Codice AREA inesistente                                                       15
056200101110Utente non abilitato all'area indicata                                        16
056300110411Obbligatorio indicare il codice AREA da elaborare                             17
