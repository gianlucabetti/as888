000100110620     /*END
000200000606     H DECEDIT('0,') DATEDIT(*DMY.)
000300120905     H* TIST04R *----------------------------------------------------*
000400120905     H* Conteggio anomalie 005 per tfp/tfa  - Filtro                 *
000500000000     H*--------------------------------------------------------------*
000700120905     FTIST04D   CF   E             WORKSTN
000800941223     D*
000900900517     D KPJBA         E DS
001000051114     D*-------------------
001100051114     D* DS PARAMETRI DI LANCIO PGM
001200051114     D*-------------------
001300120905     d tist05ds      e DS
001400090928
001500090928     d Tibs34ds      e ds
001501120905     d Tibs02ds      e ds
002000090928     d Azuteds       e ds                  Extname(Azute00f)
002100090928     d dDatiute      e ds
002300090928     d dlat          e ds
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
004700090928     D comrda          S                   LIKE(viddal) inz
004800090928     D comral          S                   LIKE(viddal) inz
004900090928     d dataisoda       s               d   datfmt(*iso)
005000090928     d dataisoa        s               d   datfmt(*iso)
005100090928     d num_days        s             15p 0
005200090928     d wabi            s                   like(UteAut)
006200090928      *   S C H I E R E
006300120906     d msg             s             79    dim(06) ctdata perrcd(1)
006600000000     C*---------------------------------------------------------------*
006700920925     C*
006800090928     c                   exsr      sr_inzd01
006900000615     C*
007000900509     C     FOR01         TAG
007100120905     C                   EXFMT     st04D01
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
008500120905     c                   clear                   tist05ds
008900120905     C                   Z-ADD     comDal        i05dal
009000120905     C                   Z-ADD     comal         i05al
010400000606     C                   CLEAR                   KPJBU
010500120905     C                   MOVEL     tist05ds      KPJBU
011300120905     C                   CALL      'TIST05R'
011400900523     C                   PARM                    KPJBA
011401120906     c                   movel     kpjbu         tist05ds
011402120906     c                   if        o05err<>' '
011403120906     c                   Eval      vidmsg = msg(06)
011404120906     c                   seton                                        28
011405120906     c                   goto      for01
011406120906     c                   endif
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
016400090928     c                   Eval      wabi = uteaut
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
021400101110     c
023700090928     c                   endsr
023800090928     C*
023900090928     C*--- Inizializzazione campi del video --------------------------*
024000090928     C     sr_inzd01     BEGSR
024100090928     c                   clear                   viddal
024200090928     c                   clear                   vidal
025500090928     c                   endsr
025600900524     C*
025700900524     C*--- CONTROLLI FORMATO1 ----------------------------------------*
025800900524     C     CONTR         BEGSR
025900120905     C                   SETOFF                                       4041
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
030100120905     c* il range tra le due date non deve superare 3gg
030300090928     c                   eval      dataisoda=%date(comdal:*iso)
030400090928     c                   eval      dataisoa=%date(comal:*iso)
030500090928     c                   eval      num_days=%diff(dataisoa: dataisoda: *days)
030600090928     c                   eval      num_days+=1
030700120905     c                   if        num_days>3
030900090928     C                   SETON                                        409028
031000090928     c                   eval      vidmsg= msg(04)
031100120905     c                   eval      vidmsg=%trim(vidmsg) + ' tre giorni'
031300090928     c                   goto      endctr
031400090928     c                   endif
031500091201     c* il range tra le due date deve essere di minimo 1 giorno lavorativo
031600120905     c***                clear                   wdat8
031700120905     c***                z-add     comdal        datada
031800120905     c***                z-add     comal         dataa
031900120905     c***                call      'XSRLAV8'
032000120905     c***                parm                    wdat8
032100120905     c***                if        ggl=0
032200120905     C***                SETON                                        409028
032300120905     c***                eval      vidmsg= msg(05)
032400120905     c***                goto      endctr
032500120905     c***                endif
054300941223     C*
054400900524     C     ENDCTR        ENDSR
054500920925     C*
054600090928**
054700120905Utente non autorizzato al lancio dell'elaborazione                            01
054800091001Data errata o mancante                                                        02
054900090928Data al minore di data dal                                                    03
055000120905Il periodo dal/al non deve essere superiore di                                04
055900120905Periodo richiesto errato: non presenti giorni lavorativi                      05
056000120906Errore Sql: elaborazione non completata                                       06
