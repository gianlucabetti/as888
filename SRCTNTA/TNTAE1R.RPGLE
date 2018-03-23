000100110620     /*END
000200000606     H DECEDIT('0,') DATEDIT(*DMY.)
000300111020     H* TNTAE1R *----------------------------------------------------*
000400111020     H* Analisi clienti con avviso al destinatario                   *
000500000000     H*--------------------------------------------------------------*
000600110620     ftabel00f  if   e           k disk
000700130808     fAZCMM01L  if   e           k disk
000800111020     FTNTAE1D   CF   E             WORKSTN
000900941223     D*
001000900517     D KPJBA         E DS
001100051114     D*-------------------
001200051114     D* DS PARAMETRI DI LANCIO PGM
001300051114     D*-------------------
001400111020     d tntae2ds      e DS
001500090928
001600090928     d Tibs34ds      e ds
001700090928     d Tibs02ds      e ds
001800090928     d Trul31ds      e ds
001900101110     d Trul31ds2     e ds
002000130808     d TRMK43ds      e ds                  inz
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
003900090928     D comdal          S                   LIKE(viddal) inz
004000090928     D comal           S                   LIKE(viddal) inz
004100090928     d dataisoda       s               d   datfmt(*iso)
004200090928     d dataisoa        s               d   datfmt(*iso)
004300090928     d num_days        s             15p 0
004400090928     d wabi            s                   like(UteAut)
004500090928     D kkut            S                   LIKE(tblkut) inz(1)
004600090928     D kcod            S                   LIKE(tblcod)
004700090928     D kkey            S                   LIKE(tblkey)
004800130808     D comage          S                   LIKE(CMMcod)
004900090928     d w003a           s              3
005000101110     D §KUT            s                   like(kkut)
005100101110     D §COD            s                   like(TBLCOD)
005200101110     D §KEY            s                   like(TBLkey)
005300101110     D §des            s             25
005400090928      *   S C H I E R E
005500110621     d msg             s             79    dim(17) ctdata perrcd(1)
005600090928     d skpog           s              3    dim(250) inz(*Zeros)
005700101110     D skARG           s              3    dim(50)
005800000000     C*---------------------------------------------------------------*
005900920925     C*
006000090928     c                   exsr      sr_inzd01
006100000615     C*
006200900509     C     FOR01         TAG
006300111020     C                   EXFMT     tae1D01
006400090928     c                   Eval      *In28 = *Off
006500090928     c                   Eval      *In90 = *Off
006600090928     c                   Clear                   vidmsg
006700941223     C*
006800941223     C** CMD3 - FINE LAVORO
006900090928     C                   if        *inkc or *in08
007000090928     c                   goto      fine
007100090928     c                   endif
007200900524     C* CONTROLLO
007300900524     C                   EXSR      CONTR
007400941223     C  NKF
007500900524     COR 90              GOTO      FOR01
007600101110     C** F6 - ELABORAZIONE
007700111020     c                   clear                   tntae2ds
007800101110     c                   if        vidare>*zeros
007900111020     c                   movel     vidare        tae2car
008000101110     c                   endif
008100111020     C                   Z-ADD     comDal        tae2dal
008200111020     C                   Z-ADD     comal         tae2al
008300111020     c                   z-add     comage        tae2age
008400111020     c   09              movel     'S'           tae2crt
008500000606     C                   CLEAR                   KPJBU
008600111020     C                   MOVEL     tntae2ds      KPJBU
008700111020     c                   eval      kcoaz = 'TAE2'
008800110719     c                   call      'BCH09'
008900110719     c                   parm                    KPJBA
009000900523     C                   CALL      'BCH10'
009100900523     C                   PARM                    KPJBA
009200920925     C*
009300920925     C     FINE          TAG
009400000000     C                   SETON                                        LR
009500090928     c*------------------------------------------------------------------------
009600090928     c     *inzsr        begsr
009700090928     C     *ENTRY        PLIST
009800090928     C                   PARM                    KPJBA
009900090928     c     *dtaara       define    §azute        azuteds
010000090928     c     *dtaara       define    §datiute      ddatiute
010100090928     c                   in(E)     *dtaara
010200090928     c                   If        %error  or RSUT = *blanks
010300090928     c                   CLEAR                   tibs34ds
010400090928     c                   CALL      'TIBS34R'
010500090928     c                   PARM                    tibs34ds
010600090928     c                   in        *dtaara
010700090928     c                   EndIf
010800091007     c*
010900091007      * Controllo se sono in sede
011000091007     c                   Eval      *In09 = (simfel = *zeros)
011100090928     c*
011200090928     c                   Clear                   wabi
011300090928     c                   Clear                   dLat
011400090928      * Verifica errori e autorità profilo
011500090928s   1c                   Select
011600090928      * se ho errori nei dati utente esco dal pgm
011700090928w   1c                   When      duterr = 'E'
011800090928     c                   Eval      *In08 = *On
011900090928     c                   Eval      *In28 = *On
012000090928     c                   Eval      vidmsg = msg(01)
012100090928     c                   Leavesr
012200090928      * se non c'è l'abilitazione
012300090928      * --> se 1° livello, abilitazioni al terminal
012400090928      *     se 2° livello, abilitazioni al punto operativo
012500090928w   1c                   When      uteaut = *Blanks
012600090928if  2c                   If        dutlpo = '1'
012700090928     c                   Eval      wabi   = 'TP'
012800090928e   2c                   EndIf
012900090928if  2c                   If        dutlpo = '2'
013000090928     c                   Eval      wabi   = 'PO'
013100090928e   2c                   EndIf
013200090928if  2c                   If        dutlpo = 'S'
013300090928     c                   Eval      wabi   = 'AZ'
013400090928e   2c                   EndIf
013500090928      * carica le abilitazioni del profilo
013600090928x   1c                   Other
013700090928     c                   Movel     utefaf        dute01
013800090928if  2c                   If        §utegtc <> *Blanks
013900090928     c                   Eval      wabi = §utegtc
014000090928   x2c                   Else
014100090928     c                   Eval      wabi = uteaut
014200090928e   2c                   EndIf
014300090928e   1c                   EndSl
014400090928
014500090928      * controllo se ok l'abilitazione dell'utente
014600090928     c                   Clear                   Tibs02ds
014700090928     c                   Eval      t02mod = 'C'
014800090928     c                   Eval      t02sif = knsif
014900090928     c                   Eval      t02cod = 'LAT'
015000090928     c                   Movel(p)  wabi          t02ke1
015100090928     c                   Call      'TIBS02R'
015200090928     c                   Parm                    kpjba
015300090928     c                   Parm                    Tibs02ds
015400090928     c                   If        t02err = *Blanks
015500090928     c                   Eval      dLat = t02uni
015600090928     c                   EndIf
015700090928      * errore o non abilitato
015800090928     c                   If        t02err <> *Blanks or §latabi = 'S'
015900090928     c                   Eval      *In08 = *On
016000090928     c                   Eval      *In28 = *On
016100090928     c                   Eval      vidmsg = msg(01)
016200090928     c                   Leavesr
016300090928     c                   EndIf
016400090928
016500090928      * Reperimento dei P.O. gestibili dall'utente per i codici clienti
016600090928     c                   Clear                   Trul31ds
016700090928     c                   Eval      i31abi = wabi
016800090928     c                   Eval      i31cdi = dutdis
016900090928     c                   Eval      i31car = dutare
017000090928     c                   Eval      i31cpo = dutpou
017100090928     c                   Call      'TRUL31R'
017200090928     c                   Parm                    kpjba
017300090928     c                   Parm                    Trul31ds
017400101110     C                   parm                    TRUL31DS2
017500090928     c                   If        o31pog > *zeros
017600090928     c                   Movea     o31pog        skpog
017700101110     c                   movea     O31arg        skarg
017800090928     c                   Else
017900090928     c                   Eval      *In08 = *On
018000090928     c                   Eval      *In28 = *On
018100090928     c                   Eval      vidmsg = msg(01)
018200090928     c                   Leavesr
018300090928     c                   EndIf
018400101110     c
018500101110     c* Se utente abilitato ad almeno 2 aree, abilito la richiesta dell' area
018600101110     c                   if        skarg(2)>*zeros
018700101110     c                   seton                                        15
018800101110     c                   endif
018900090928     c*
019000090928     c* reperimento data ultima pulizia titasp
019100090928     c                   clear                   ds$3
019200090928     C                   MOVEL     '$3'          KCOD
019300090928     C                   MOVEL     '1       '    KKEY
019400090928     c     ktab          chain     tabel00f
019500090928     c                   if        %found(tabel00f)
019600090928     c                   movel     tbluni        ds$3
019700090928     c                   endif
019800090928     C*  KLIST
019900090928     C     KTAB          KLIST
020000090928     C                   KFLD                    KKUT
020100090928     C                   KFLD                    KCOD
020200090928     C                   KFLD                    KKEY
020300090928     c                   endsr
020400090928     C*
020500090928     C*--- Inizializzazione campi del video --------------------------*
020600090928     C     sr_inzd01     BEGSR
020700090928     c                   clear                   viddal
020800090928     c                   clear                   vidal
020900090928     c                   clear                   vidage
021000090928     c                   clear                   vidaged
021100101110     c                   clear                   vidare
021200101110     c                   clear                   vidared
021300090928     c                   endsr
021400900524     C*
021500900524     C*--- CONTROLLI FORMATO1 ----------------------------------------*
021600900524     C     CONTR         BEGSR
021700111020     C                   SETOFF                                       404144
021800111020     C                   SETOFF                                       47
021900090928     C* DATA elaborazione obbligatoria
022000090928     c                   clear                   wlbdat
022100090928     C                   MOVE      VIDDal        G02DAT
022200920925     C                   MOVEL     *BLANK        G02ERR
022300941223     C                   CALL      'XSRDA8'
022400920925     C                   PARM                    WLBDAT
022500920925     C     G02ERR        IFEQ      '1'
022600090928     C                   SETON                                        402890
022700090928     c                   eval      vidmsg=msg(02)
022800920925     C                   GOTO      ENDCTR
022900920925     C                   END
023000090928     C                   Z-ADD     G02INV        comdal
023100941223     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
023200090928     C                   Z-ADD     G02DAT        VIDDal
023300900524     C*
023400090928    1c                   if        vidal>0
023500090928     c                   clear                   wlbdat
023600090928     C                   MOVE      VIDal         G02DAT
023700051114     C                   MOVEL     *BLANK        G02ERR
023800051114     C                   CALL      'XSRDA8'
023900051114     C                   PARM                    WLBDAT
024000051114     C     G02ERR        IFEQ      '1'
024100090928     C                   SETON                                        412890
024200090928     c                   eval      vidmsg=msg(02)
024300051114     C                   GOTO      ENDCTR
024400051114     C                   END
024500090928     C                   Z-ADD     G02INV        comal
024600051114     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
024700090928     C                   Z-ADD     G02DAT        VIDal
024800090928     c                   else
024900090928     c                   z-add     comdal        comal
025000090928    1c                   endif
025100941223     C*
025200900524     C* DATA DAL NO MAGGIORE DATA AL
025300090928     C     comdal        IFGT      comal
025400090928     C                   SETON                                        409028
025500090928     c                   eval      vidmsg= msg(03)
025600900524     C                   GOTO      ENDCTR
025700900524     C                   END
025800111024     c* il range tra le due date non deve superare 32gg
025900090928     c                   eval      dataisoda=%date(comdal:*iso)
026000090928     c                   eval      dataisoa=%date(comal:*iso)
026100090928     c                   eval      num_days=%diff(dataisoa: dataisoda: *days)
026200090928     c                   eval      num_days+=1
026300111024     c                   if        (num_days>32)
026400090928     C                   SETON                                        409028
026500090928     c                   eval      vidmsg= msg(04)
026600111024     c                   eval      vidmsg=%trim(vidmsg) + ' trentadue giorni'
026700090928     c                   goto      endctr
026800090928     c                   endif
026900091201     c* il range tra le due date deve essere di minimo 1 giorno lavorativo
027000091201     c                   clear                   wdat8
027100091201     c                   z-add     comdal        datada
027200091201     c                   z-add     comal         dataa
027300091201     c                   call      'XSRLAV8'
027400091201     c                   parm                    wdat8
027500091201     c                   if        ggl=0
027600091201     C                   SETON                                        409028
027700091201     c                   eval      vidmsg= msg(13)
027800091201     c                   goto      endctr
027900091201     c                   endif
028000090928     c* errore se superati i giorni di pulizia di titsp per il periodo richiesto
028100090928     c                   if        comdal<=§$3dp0
028200090928     C                   SETON                                        409028
028300090928     c                   eval      vidmsg= msg(06)
028400090928     c                   goto      endctr
028500090928     c                   endif
028600101110     c
028700101110     c* Commerciale e area in alternativa
028800101110     c                   if        vidare<>*blanks and vidare<>*zeros and
028900101110     c                             vidage<>*blanks and vidage<>*zeros
029000101110     c                   seton                                        442890
029100101110     c                   eval      vidmsg=msg(14)
029200101110     c                   goto      endctr
029300101110     c                   endif
029400110411     c
029500110411     c* Se non immesso il commerciale, area obbligatoria se utente abilitato
029600110411     c*    all'azienda
029700110411     c                   if        wabi='AZ' and dutpou<>046
029800110411     c                   if        (vidage=*blanks or  vidage=*zeros) and
029900110411     c                             (vidare=*blanks or  vidare=*zeros)
030000110411     c                   seton                                        472890
030100110411     c                   eval      vidmsg=msg(17)
030200110411     c                   goto      endctr
030300110411     c                   endif
030400110411     c                   endif
030500101110     c* AREA
030600101110     c* interrogazione (?)
030700101110     c                   if        %scan('?':vidare)>0
030800101110     c                   movel     kkut          §kut
030900101110     c                   movel     '05'          §cod
031000101110     c                   clear                   §key
031100101110     c                   call      'X§TABER'
031200101110     c                   parm                    §kut
031300101110     c                   parm                    §cod
031400101110     c                   parm                    §key
031500101110     c                   parm                    §des
031600101110     c                   movel     §key          vidare
031700101110     c                   movel     §des          vidared
031800101110     C                   EVAL      *IN90=*ON
031900101110     C                   EVAL      *IN47=*ON
032000101110     C                   GOTO      ENDctr
032100101110     c                   endif
032200101110     c* Controllo in tabella
032300101110     c                   clear                   vidared
032400101110     c                   if        vidare<>*blanks and vidare<>*zeros
032500101110     c                   movel     '05'          kcod
032600101110     c                   movel(p)  vidare        kkey
032700101110     c     ktab          chain     tabel                              30
032800101110    2c                   if        *in30 OR tblflg<>' '
032900101110     c                   seton                                        472890
033000101110     c                   eval      vidmsg=msg(15)
033100101110     c                   goto      endctr
033200101110     c                   endif
033300101110     c                   movel     tbluni        vidared
033400101110     c* Controllo se utente abilitato
033500101110     c     vidare        lookup    skarg                                  30
033600101110    2c                   if        not *in30
033700101110     c                   seton                                        472890
033800101110     c                   eval      vidmsg=msg(16)
033900101110     c                   goto      endctr
034000101110     c                   endif
034100101110     c                   endif
034200101110     c
034300101110     c
034400090928     c* COMMERCIALE UNIFICANTE
034500090929     c* interrogazione (?)
034600090929     c                   if        %scan('?':vidage)>0
034700130808     c                   clear                   TRMK43ds
034800130808     c                   eval      iMK43solU = 'S'
034900130808     C                   CALL      'TRMK43R'
035000090929     C                   PARM                    KPJBA
035100130808     C                   PARM                    TRMK43ds
035200130808     c                   if        oMK43err = *off  and
035300130808     c                             oMK43fxx = *blank
035400130808     C                   MOVEL     oMK43cmm      vidage
035500130808     C                   MOVEL     oMK43des      vidaged
035600130808     c                   endif
035700090929     C                   EVAL      *IN90=*ON
035800090929     C                   GOTO      ENDctr
035900090929     c                   endif
036000130808     c*
036100090928     c                   clear                   vidaged
036200111024     c                   clear                   comage
036300130808    1c                   IF        vidage<>*zeros and vidage<>*blanks
036400090929     c* deve contenere numeri
036500090929     c                   eval      *in10   = *off
036600090929     c                   testn                   vidage               10
036700090929    2c                   if             *in10  = *off
036800090929     c                             or  %subst(vidage: 7: 1) < *zero
036900090929     c                   seton                                        449028
037000090929     c                   eval      vidmsg=msg(7)
037100090929     c                   goto      endctr
037200090929    2c                   endif
037300090929     c                   move      vidage        comage
037400090928     c* la filiale cliente deve essere una filiale gestita dall'utente
037500090928     c                   movel     vidage        w003a
037600090929    2c                   if        %lookup(w003a:skpog)=0
037700090928     c                   seton                                        449028
037800090928     c                   eval      vidmsg=msg(8)
037900090928     c                   goto      endctr
038000090929    2c                   endif
038100130808      *
038200130808     c     ComAge        chain     AZCMM000
038300130808    2c                   if        not %found (AZCMM01L)
038400090928     c                   seton                                        449028
038500090928     c                   eval      vidmsg=msg(7)
038600090928     c                   goto      endctr
038700090929    2c                   endif
038800130808     c                   movel     CMMdes        vidaged
038900130808    2c                   if        CMMuni <> comage
039000090928     c                   seton                                        449028
039100090928     c                   eval      vidmsg=msg(9)
039200090928     c                   goto      endctr
039300090929    2c                   endif
039400130808    1c                   ENDIF
039500941223     C*
039600900524     C     ENDCTR        ENDSR
039700920925     C*
039800090928**
039900111020Utente non autorizzato all'Analisi                                            01
040000091001Data errata o mancante                                                        02
040100090928Data al minore di data dal                                                    03
040200111024Il periodo dal/al non  deve essere superiore di                               04
040300111020                                                                              05
040400090928Periodo non elaborabile per mancanza di dati                                  06
040500090928Codice Commerciale errato o mancante                                          07
040600090928Utente non abilitato al codice immesso                                        08
040700090928Il codice immesso non è quello di un commerciale unificante                   09
040800111020                                                                              10
040900111020                                                                              11
041000111020                                                                              12
041100091201Periodo richiesto errato: non presenti giorni lavorativi                      13
041200101110Immettere o l'area o il commerciale unificante                                14
041300101110Codice AREA inesistente                                                       15
041400101110Utente non abilitato all'area indicata                                        16
041500110411Obbligatorio indicare il codice AREA da elaborare                             17
