000100110801     /*END
000200010628     H DECEDIT('0,') DATEDIT(*YMD.)
000300100210      ***************************************************************************
000400100224      * Tabella DCR
000500100210      *  clienti con calcolo in immissione bolle della data consegna richiesta
000600110803      ***************************************************************************
000700110803
000800110803      *?  ATTENZIONE!!  ?
000900110803      *?    Questa tabella è utilizzata anche dal pgm TNTA61R  ?
001000110803      *?    'Interrogazione abilitazioni clienti'              ?
001100110803      *?    In caso di aggiunta/modifica campi alla tabella    ?
001200110803      *?    verificare se sono validi per la visualizzazione   ?
001300110803      *?    da TA61                                            ?
001400110803
001500100210      ***************************************************************************
001600010628      *  RIEPILOGO INDICATORI
001700010628      ****************************************************************
001800010628      * 01 - RECORD ANNULLATO
001900010628      * 02 - *ON VARIAZIONE - *OFF IMMISSIONE
002000010628      * 28 - ERRORE GENERICO DSPF
002100010628      * 31 - GENERICO
002200010628      * 90 - RIEMISSIONE VIDEATA
002300010628      ****************************************************************
002400100224     Ftntam01L  IF   E           K DISK
002500100224     FTNTBE01L  IF   E           K DISK
002600100210     F                                     INFDS(tbe01)
002700100210     FTNTBE00f  UF A E             DISK    rename(tntbe000:tntbe0fis)
002800100210     Fcnaco00f  IF   E           K DISK
002900100210     FTNTB80D   CF   E             WORKSTN
003000100212     F                                        SFILE(tntb80s2:sflNRR)
003100010628      *------------------------------------------------------------------------*
003200100210     D TBE01           DS
003300100210     D  tbeNRR               397    400B 0
003400100210     D KCOD            S                   LIKE(TBECOD) inz('DCR')
003500010628     D KKE1            S                   LIKE(TBEKE1)
003600010628      *   S C H I E R E
003700100224     D MSG             S             78    DIM(9) CTDATA PERRCD(1)              MSG VIDEO
003800100210
003900100210     D ctrpa           S              3    DIM(15)
004000100210     D ore             S              4    DIM(15)
004100100211     D nrr             S              5  0 DIM(15)
004200100210     D*
004300010628     D TIBS02DS      E DS
004400100211     D DDCR          E DS
004500100210      *
004600100210     d TIBS34ds      e ds
004700100210     d dDatiUte      e ds
004800100210     d AZUTEds       e ds                  extname(AZUTE00F)
004900100210      *
005000010628     D*
005100010628     D KPJBA         E DS
005200110802     d TNTA61ds      e ds
005300110802     d*
005400100210     D PARAM4          DS
005500100210     D  PA4COD                 6     12  0
005600100210     D  PA4CTR                13     15
005700100210     D  PA4FLG                16     16
005800100210     D  PA4VPR                17     17
005900100210     D  PA4CCC                18     21  0
006000100210     D  PA4DCV                22     36
006100100210     d  pa4prg                37     39
006200100210     d  pa4dtr                40     47
006300010628     D*
006400100210     d pardut          s             30
006500100210     d parrag          s             48
006600100210     d parkcc          s              4  0
006700100210     d parsta          s              1  0
006800100210     d parflr          s             90
006900100210     d pardit          s              3
007000100210     d parnum          s              2  0
007100100210     d parkcm          s             80
007200100210     d parksm          s            140
007300100210     d parkdm          s             60
007400100210     d paresci         s              1
007500100210     d parerr          s              2
007600100210     d pariva          s             16
007700100210     d parcdf          s             16
007800100210     d
007900100210     d kksc            s                   like(acoksc)
008000100224     d kctr            s                   like(tamctr)
008100100210     d xx              s              2  0
008200100210     d codut           s              1  0 inz(1)
008300100211     d wrem            s              3  0
008400100212     d sflnrr          s              4  0
008500100210     d
008600100210     d Datasys         s               d   inz(*sys) datfmt(*iso)
008700110802       // -?Costante per controllo "caratteri solo numerici"?
008800110802     d c_Digits        c                   const('0123456789')
008900010628     D*------------------------------------------------------------------------*
009000110801     c                   eval      *in10 = *on
009100110802     c                   Select
009200110802     c                   When      %parms() > 1  and  TA61ksc <= *zero
009300110802     c                   eval      TA61err = *on
009400110802     c                   eval      TA61msg = 'Richiesto il codice cliente'
009500110802     c                   goto      Fine
009600110802     c                   When      %parms() > 1
009700110802     c                   eval      V1Cksc  = %editc(TA61ksc:'X')
009800110801     c                   eval      *in10 = *off
009900110801     c                   setoff                                       2890
010000110801     c                   exsr      sr_Contr01
010100110810     c                   IF        *in90
010200110810     c                   eval      TA61err = *on
010300110810     c                   eval      TA61msg = V1Cmsg
010400110810     c                   goto      Fine
010500110810     c                   endif
010600110810     c                   goto      Car02
010700110802     c                   EndSl
010800010628     C* Pulizia 1° Videata
010900100210     c     inizio        tag
011000100210     C                   MOVE      *BLANKS       V1Cksc
011100100210     C                   MOVE      '?'           V1Cksc
011200100210     C                   MOVE      *BLANKS       V1dksc
011300100224     c                   clear                   sflnrr
011400010628     C*
011500010628      * Emissione 1° Videata
011600100210     c     FOr01         tag
011700100210     C                   EXFMT     TNTB801
011800010628      * F3=Fine
011900100210     C   KC              goto      fine
012000010628     C*
012100010628      * Controlli 1° videata
012200100210     C                   SETOFF                                       2890
012300010628     C                   EXSR      SR_CONTR01
012400010628     C*
012500100210     C   90              goto      for01
012600010628     C*
012700110801     c     Car02         tag
012800110801      *
012900010628      * Caricamento e pulizia seconda videata
013000010628     C                   EXSR      SR_CARVID
013100110801     c*
013200100211     c* Emetto sfl se trovato almeno un record
013300100211     c                   if        xx>0
013400100211     C     for02         TAG
013500100212     c                   write     tntb80z2
013600100211     C                   EXFMT     TNTB80c2
013700100211     C*
013800100211     c                   setoff                                       9028
013900100211     C*
014000100211      * F3=Fine
014100100211     C   KC              goto      fine
014200100211     C*
014300100211      * F12=Ritorno
014400110801     C*//KL              goto      for01
014500110803     c   KL              if        *in10
014600110801     c                   goto      For01
014700110801     c                   else
014800110801     c                   goto      Fine
014900110801     c                   endif
015000110801     c*
015100100211     c* se effettuata scelta, o "F10-Nuova Immissione" -->  emetto video 3
015200100212     c                   readc     tntb80s2
015300110802      *
015400100212     c
015500100212     c                   if        v2csce='1'  or *inkj=*on
015600100211     c                   exsr      Caricavid3
015700100211     c                   else
015800100211     c                   goto      for02
015900100211     c                   endif
016000100211     c
016100100211     c                   endif
016200010628     C*
016300100210      * Emissione 3° Videata
016400100210     C     for03         TAG
016500100210     C                   EXFMT     TNTB803
016600010628     C*
016700100210     c                   setoff                                       9028
016800100212     c                   setoff                                       404142
016900010628     C*
017000010628      * F3=Fine
017100100210     C   KC              goto      fine
017200010628     C*
017300010628      * F12=Ritorno
017400100224     c                   if        *inkl
017500100224     c                   if        sflnrr=0
017600100224     C                   goto      for01
017700100224     c                   else
017800100224     C                   goto      for02
017900100224     c                   endif
018000100224     c                   endif
018100010628     C*
018200100210     C                   EXSR      SR_CONTR03
018300100210     C   90              GOTO      for03
018400010628     C*
018500010628      * F6=Aggiornamento o F5=Ripristino o F16=Annullamento
018600010628     C                   IF        *INKE = *ON OR *INKF = *ON OR *INKQ = *ON
018700010629     C                   EXSR      SR_REGISTRA
018800100210     C                   goto      inizio
018900010628     C                   ENDIF
019000010628     C*
019100100210     C                   GOTO      for03
019200040625     c*
019300100210     c     fine          tag
019400040625     c                   clear                   tibs02ds
019500040625     c                   movel     'C'           t02tla
019600040625     c                   call      'TIBS02R'
019700040625     c                   parm                    kpjba
019800040625     c                   parm                    tibs02ds
019900010628     C*
020000010628     C                   EVAL      *INLR = *ON
020100010628      **********************************************************************
020200010628      * CONTROLLI VIDEO 1
020300010628      **********************************************************************
020400010628     C     SR_CONTR01    BEGSR
020500010628      * Reset indicatori
020600100210     C     '?'           SCAN      V1Cksc                                 31
020700010628     C     *IN31         IFEQ      *ON
020800010628     C*
020900010628     C                   EVAL      *IN90 = *ON
021000010628     C                   CLEAR                   TIBS02DS
021100010628     C                   MOVEL     'R'           T02MOD
021200010628     C                   MOVEL     KNSIF         T02SIF
021300100210     C                   MOVEL     'DCR'         T02COD
021400010628     C                   CALL      'TIBS02R'
021500010628     C                   PARM                    KPJBA
021600010628     C                   PARM                    TIBS02DS
021700010628     C                   IF        T02ERR = *BLANKS
021800100210     C                   MOVEL     T02KE1        V1Cksc
021900040625     c                   else
022000100210     c                   clear                   v1cksc
022100010628     C                   SETON                                        90
022200100210     C                   GOTO      ENDSRCONTR
022300010628     C                   ENDIF
022400010628     C                   ENDIF
022500010628
022600100210     C* Verifico se richiesta la ricerca alfabetica
022700100210     c                   if        v1dKSC<>*blanks and v1cksc<=*zeros
022800100210     c                   eval      pardut=rsut
022900100210     c                   clear                   parrag
023000100210     c                   eval      parrag=v1dKSC
023100100210     c                   eval      parkcc=dutkci
023200100210     c                   eval      parsta=*zeros
023300100210     c                   clear                   parflr
023400100210     c                   clear                   pardit
023500100210     c                   eval      parnum=1
023600100210     c                   clear                   parkcm
023700100210     c                   clear                   parksm
023800100210     c                   clear                   parkdm
023900100210     c                   clear                   paresci
024000100210     c                   clear                   parerr
024100100210     c                   clear                   pariva
024200100210     c                   clear                   parcdf
024300100210     c                   call      'XALFA3BR'
024400100210     C                   PARM                    PARDUT
024500100210     C                   PARM                    codut
024600100210     C                   PARM                    parrag
024700100210     C                   PARM                    parkcc
024800100210     C                   PARM                    PARSTA
024900100210     C                   PARM                    PARFLR           90
025000100210     C                   PARM                    PARDIT            3
025100100210     C                   PARM                    PARNUM
025200100210     C                   PARM                    PARKCM
025300100210     C                   PARM                    PARKSM
025400100210     C                   PARM                    PARKDM
025500100210     C                   PARM                    PAResci
025600100210     C                   PARM                    PARerr
025700100210     C                   PARM                    PARiva
025800100210     C                   PARM                    PARcdf
025900100210     c
026000100210     c                   if        parsta<>-1 and parerr=*blanks
026100100210     c                   eval      v1cksc=parksm
026200100210     c                   eval      v1dksc=parrag
026300100210     c                   endif
026400100210
026500100210     c                   seton                                        90
026600100210     C                   GOTO      ENDSRCONTR
026700100210     c                   endif
026800100210     c
026900100210      * Controllo se campo valorizzato
027000100210     C                   IF        V1Cksc<= *ZEROS
027100100224     C                   SETON                                        2890
027200010628     C                   MOVEL     MSG(1)        V1CMSG
027300010628     C                   GOTO      ENDSRCONTR
027400010628     C                   ENDIF
027500100210     c
027600100210     c* controllo ksc
027700100210     c     c_digits      check     v1cksc                                 30
027800100211     c                   if        *in30
027900100210     C                   SETON                                        2890
028000100210     C                   MOVEL     MSG(2)        V1CMSG
028100100210     C                   GOTO      ENDSRCONTR
028200100210     C                   ENDIF
028300100210     c
028400100210     c                   movel     v1cksc        kksc
028500100210     c     kaco          chain     cnaco00f
028600110810
028700110810      * in caso di richiamo per interrogazione verifico solo l'esistenza
028800110810      * del codice cliente su CNACO
028900110810     c                   IF        not *in10
029000110810     c                   IF        not %found(cnaco00f)
029100110810     c                   seton                                        2890
029200110810     c                   eval      V1Cmsg = 'Cliente non trovato'
029300110810     c                   goto      ENDSRCONTR
029400110810     c                   ENDIF
029500110810     c                   ELSE
029600100210     c                   if        not %found(cnaco00f) or acoabl<>*blanks
029700100210     C                   SETON                                        2890
029800100210     C                   MOVEL     MSG(3)        V1CMSG
029900100210     C                   GOTO      ENDSRCONTR
030000100210     C                   ENDIF
030100110804     c                   ENDIF
030200110804
030300010628     C*
030400010628     C     ENDSRCONTR    endsr
030500010628      **********************************************************************
030600100210      * CARICAMENTO dati del cliente
030700010628      **********************************************************************
030800010628     C     SR_CARVID     BEGSR
030900100210     c                   setoff                                       0102
031000100211     c
031100100211     C                   SETON                                        35
031200100211     C                   WRITE     tntb80c2
031300100211     C                   SETOFF                                       35
031400100210     c
031500100210     c* Leggo tutti i codici che trovo per ogni codice cliente
031600100212     c                   clear                   sflnrr
031700100212     c                   clear                   xx
031800100210     c                   clear                   ctrpa
031900100210     c                   clear                   ore
032000100212     c                   clear                   v2hnrr
032100100211     c                   clear                   v3cctr
032200100211     c                   clear                   v3ctcr
032300100211     c                   clear                   v3cpart
032400100211     c                   clear                   v3core
032500040625     c*
032600100210     C                   MOVEL(p)  V1Cksc        KKE1
032700100210     C                   MOVEL(p)  V1Cksc        kksc
032800100210     c
032900100210     C     Ktbe          setll     TNTBE01L
033000100210     C     ktbe          reade     TNTBE01L
033100100212     c
033200100210     c                   dow       not %eof(tntbe01l)
033300100210     c
033400100210     c                   eval      xx=xx+1
033500100210     c                   eval      ctrpa(xx)=%subst(tbeke2:1:3)
033600100210     c                   eval      ore(xx)=%subst(tbeuni:1:4)
033700100211     c                   eval      nrr(xx)=tbenrr
033800100211     c                   exsr      Caricasfl
033900100210     c
034000100210     C     ktbe          reade     TNTBE01L
034100100210     c                   enddo
034200010628     C*
034300100212     c                   clear                   v2cdes
034400100212     c                   clear                   v2cctr
034500100210     c                   clear                   v2cpart
034600100210     c                   clear                   v2ctcr
034700100210     c                   clear                   v2core
034800100210     c                   movel     acorag        v2dksc
034900100210     c
035000100210     c                   if        xx=0
035100100212     C                   MOVEL     'IMMISSIONE'  V3vAIM
035200100210     c                   seton                                        01
035300100211     c                   endif
035400010628     C*
035500010628     C                   ENDSR
035600100211      **********************************************************************
035700100211      * Carica SFL
035800100211      **********************************************************************
035900100211     C     Caricasfl     BEGSR
036000100211     c
036100100212     c                   clear                   v2cpart
036200100212     c                   clear                   v2cctr
036300100212     c                   select
036400100212     c                   when      ctrpa(xx)='TTR'
036500100212     c                   eval      v2cpart='SI'
036600100212     c                   eval      v2cdes='Per Particolarità CONSEGNA'
036700100212
036800100212     c                   when      ctrpa(xx)>=*zeros
036900100212     c                   eval      v2cctr=ctrpa(xx)
037000100212     c                   eval      v2cdes='Per Codice Tariffa: '+ v2cctr
037100100212     c                   other
037200100212
037300100212     c                   eval      v2cdes='Per tutte le bolle'
037400100212     c                   endsl
037500100211
037600100211     c                   movel     ore(xx)       v2ctcr
037700100211     c                   move      ore(xx)       v2core
037800100212     c                   move      nrr(xx)       v2hnrr
037900100211     c                   select
038000100211     c                   when      v2ctcr=' '
038100100211     c                   eval      v2dtcr='   IL    '
038200100211     c                   when      v2ctcr='D'
038300100211     c                   eval      v2dtcr=' DOPO IL '
038400100211     c                   when      v2ctcr='P'
038500100211     c                   eval      v2dtcr='PRIMA DEL'
038600100211     c                   endsl
038700100212     c
038800100212     c                   if        tbeatb<>' '
038900100212     c                   eval      v2cann='ANNULLATO'
039000100212     c                   seton                                        04
039100100212     c                   else
039200100212     c                   clear                   v2cann
039300100212     c                   setoff                                       04
039400100212     c                   endif
039500100211     c
039600100212     c                   eval      sflnrr=sflnrr+1
039700100211     c                   write     tntb80s2
039800100211     c
039900100211     C                   ENDSR
040000100211      **********************************************************************
040100100211      * carica dati record scelto nella videata singola
040200100211      **********************************************************************
040300100211     c     Caricavid3    BEGSR
040400100224     c                   if        not *inkj and v2csce='1'
040500100212     c                   clear                   v2csce
040600100212     c                   if        v2cann<>*blanks
040700100212     c                   seton                                        04
040800100212     c                   else
040900100212     c                   setoff                                       04
041000100212     c                   endif
041100100212     c                   update    tntb80s2
041200100224     c                   endif
041300100211     c
041400100211     c                   if        *inkj=*on
041500100211     c                   clear                   v3cctr
041600100211     c                   clear                   v3cpart
041700100211     c                   clear                   v3core
041800100211     c                   clear                   v3ctcr
041900100212     c                   clear                   v2hnrr
042000100211     c                   seton                                        01
042100100212     c                   setoff                                       0203
042200100211     C                   MOVEL     'IMMISSIONE'  V3VAIM
042300100211     c                   else
042400100211     c
042500100212     c     sflnrr        chain     tntb80s2
042600100211     c
042700100211     c                   if        %found
042800100211     c*  Se presente solo uno emetto videata di modifica altrimenti di scelta
042900100211     c                   seton                                        02
043000100212     c                   setoff                                       0103
043100100212     c                   if        v2cann=*blanks
043200100211     C                   MOVEL     'VARIAZIONE'  V3VAIM
043300100212     c                   else
043400100212     C                   MOVEL     'ANNULLATO '  V3VAIM
043500100212     c                   seton                                        03
043600100212     c                   endif
043700100211     c
043800100211     c                   eval      v3cctr =v2cctr
043900100211     c                   eval      v3cpart=v2cpart
044000100211     c                   eval      v3core =v2core
044100100211     c                   eval      v3ctcr =v2ctcr
044200100211     c                   endif
044300100212     c                   endif
044400100211     c
044500100211     C                   ENDSR
044600010628      **********************************************************************
044700010628      * CONTROLLI VIDEO 2
044800010628      **********************************************************************
044900100210     C     Sr_Contr03    begsr
045000100210     c
045100100210     c* Ricerca col '?' sul codice tariffa
045200100212     C     '?'           SCAN      V3Cctr                                 31
045300100210     c
045400100210     C     *IN31         IFEQ      *ON
045500100210     C                   CLEAR                   PARAM4
045600100210     C                   MOVEL     v1cksc        PA4COD
045700100210     C                   Z-ADD     dutkci        PA4CCC
045800100210     c                   movel     '2'           pa4vpr
045900100210     c                   move      dateu         pa4dtr
046000100210     C                   MOVEL     PARAM4        KPJBU
046100100210     C                   CALL      'TNTA36R'
046200100210     C                   PARM                    KPJBA
046300100210     C                   MOVEL     KPJBU         PARAM4
046400100210     C*
046500100210    2C     PA4CTR        IFNE      *BLANKS
046600100212     c                   movel     pa4ctr        v3cctr
046700100211     c                   seton                                        9040
046800100211     c                   goto      endct3
046900100210     c                   endif
047000100210     c                   endif
047100100224     c
047200100323     c* Se immesso codice tariffa, verifico se esiste almeno un record (non mi interessa
047300100224     c*  se scaduto, decorrente o ancora da decorrere...)
047400100224     c                   if        v3cCtr<>*blanks
047500100224     c                   if        v3cctr<*zeros
047600100224     C                   MOVEL     MSG(8)        V1CMSG
047700100224     c                   seton                                        904028
047800100224     c                   goto      endct3
047900100224     c                   endif
048000100224     c
048100100224     c     c_digits      check     v3cctr                                 30
048200100224     c                   if        *in30
048300100224     C                   MOVEL     MSG(8)        V1CMSG
048400100224     c                   seton                                        904028
048500100224     c                   goto      endct3
048600100224     c                   endif
048700100224     c
048800100224     c                   movel     v3cctr        kctr
048900100224     c     ktam          setll     tntam01l
049000100224     c     ktam          reade     tntam01l
049100100224     c                   dow       not %eof(tntam01l) and tamatb<>' '
049200100224     c     ktam          reade     tntam01l
049300100224     c                   enddo
049400100224     c
049500100224     c                   if        %eof(tntam01l)
049600100224     C                   MOVEL     MSG(9)        V1CMSG
049700100224     c                   seton                                        904028
049800100224     c                   goto      endct3
049900100224     c                   endif
050000100224     c                   endif
050100100211     c
050200100211     c* Cod tariffa e particolarità sono in alternativa
050300100212     c                   if        v3cctr<>*blanks and v3cpart<>'  '
050400100211     C                   MOVEL     MSG(5)        V1CMSG
050500100212     c                   seton                                        904128
050600100211     c                   goto      endct3
050700100211     c                   endif
050800100212     c
050900100212     c* Verifico se per quel cliente esiste già un altro record
051000100212     c                   z-add     1             xx
051100100212    1c                   dow       nrr(xx)>0
051200100212    2c                   if        nrr(xx)<>v2hnrr
051300100212     c*
051400100212    3c                   select
051500100212     c                   when      ctrpa(xx)=*blanks and v3cpart='  '
051600100212     c                             and v3cctr=*blanks
051700100212     C                   MOVEL     MSG(6)        V1CMSG
051800100212     c                   seton                                        904128
051900100212     c                   goto      endct3
052000100212     c
052100100212     c                   when      (ctrpa(xx)='TTR' and v3cpart='SI')  or
052200100212     c                             (ctrpa(xx)>=*zeros and ctrpa(xx)=v3cctr)
052300100212     C                   MOVEL     MSG(6)        V1CMSG
052400100212     c                   seton                                        904128
052500100212     c                   goto      endct3
052600100212
052700100212     c                   when      ctrpa(xx)<>*blanks and (v3cpart='SI'  or
052800100212     C                              V3CCTR<>*BLANKS)
052900100212     C                   MOVEL     MSG(7)        V1CMSG
053000100212     c                   seton                                        904128
053100100212     c                   goto      endct3
053200100212    3c                   endsl
053300100212    2c                   endif
053400100212     c                   eval      xx=xx+1
053500100212    1c                   enddo
053600100211     c
053700100211     C* ORE: obbbligatorie e multiplo di 12
053800100212     c                   if        v3core=0
053900100211     C                   MOVEL     MSG(4)        V1CMSG
054000100212     c                   seton                                        904228
054100100211     c                   goto      endct3
054200100211     c                   endif
054300100211     c
054400100211     c                   eval      wrem=%rem(v3core:12)
054500100211     c                   if        wrem>0
054600100211     C                   MOVEL     MSG(4)        V1CMSG
054700100212     c                   seton                                        904228
054800100211     c                   goto      endct3
054900100211     c                   endif
055000100211     c
055100100212     c
055200100211     C     ENDCT3        ENDSR
055300010628      **********************************************************************
055400010628      * REGISTRAZIONE DATI IMMESSI
055500010628      **********************************************************************
055600010628     C     SR_REGISTRA   BEGSR
055700100211     c
055800100212     c* record nuovo
055900100211     c                   if        *in01
056000100211     c                   clear                   tntbe0fis
056100100211     c                   eval      tbecod='DCR'
056200100211     c                   eval      tbeapl='OP'
056300100211     c                   else
056400100211     c
056500100211     c* Se record esistente, chaino il fisico
056600100212     c     v2hnrr        chain     tntbe00f
056700100212     c                   endif
056800100211     c
056900100211     c                   eval      tbeke1=v1cksc
057000100211
057100100211     c                   clear                   tbeke2
057200100211     c                   select
057300100211     c                   when      v3cctr<>*blanks
057400100212     c                   eval      tbeke2=v3cctr
057500100211     c                   when      v3cpart<>*blanks
057600100211     c                   eval      tbeke2='TTR'
057700100211     c                   endsl
057800100211     c
057900100211     c                   clear                   ddcr
058000100211     c                   eval      §dcrtcr=v3ctcr
058100100211     c                   eval      §dcrore=v3core
058200100211     c                   movel(p)  ddcr          tbeuni
058300100211     c
058400100211     c                   clear                   tbeftr
058500100211     c                   eval      tbeftt='S'
058600100212     c* Annullamento
058700100224     c                   if        *inkq=*on
058800100212     c                   eval      tbeatb='A'
058900100212     c                   endif
059000100212     c* Ripristino
059100100212     c                   if        *inke=*on
059200100212     c                   eval      tbeatb=' '
059300100212     c                   endif
059400100211     c
059500100211     c                   if        *in01
059600100211     c                   write     tntbe0fis
059700100211     c                   else
059800100211     c                   update    tntbe0fis
059900100211     c                   endif
060000010628
060100040625     C                   ENDSR
060200010628      *****************************************************************
060300010628      * ROUTINE INIZIALE
060400010628      *****************************************************************
060500010628     C     *INZSR        BEGSR
060600010628      *
060700010628     C     *ENTRY        PLIST
060800010628     C                   PARM                    KPJBA
060900110802     c                   parm                    TNTA61ds
061000100210      *
061100100210      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
061200100210     c     *dtaara       define    §azute        azuteds
061300100210     c     *dtaara       define    §datiute      ddatiute
061400100210      *
061500100210     c                   clear                   AzUteDs
061600100210     c                   clear                   DDatiUte
061700100210     c                   clear                   Tibs34Ds
061800100210     c                   in(E)     *dtaara
061900100210if  1c                   if        %Error  or  RSUT = *blanks
062000100210     c                   call      'TIBS34R'
062100100210     c                   parm                    Tibs34Ds
062200100210     c                   in        *dtaara
062300100210e   1c                   endif
062400110801     c*
062500100210     c     *iso          movel     datasys       dateu             8 0
062600010628     C*
062700100210     C     Ktbe          KLIST
062800010628     C                   KFLD                    KCOD
062900010628     C                   KFLD                    KKE1
063000100210     C     Kaco          KLIST
063100100210     C                   KFLD                    codut
063200100210     C                   KFLD                    dutkci
063300100210     C                   KFLD                    kksc
063400100224     C     KTAM          KLIST
063500100224     C                   KFLD                    kksc
063600100224     C                   KFLD                    kctr
063700010628     C*
063800010628     C                   endsr
063900010628      *---------------------------------------------------------------------------------------------
064000010628** MSG  Lungh. 78                                                            *
064100100210Codice Cliente obbligatorio                                                   1
064200100211Codice Cliente errato                                                         2
064300100211Codice Cliente non trovato nel piano dei conti o bloccato                     3
064400100212Numero di ore obbligatorio: deve essere un multiplo di 12                     4
064500100211Immettere O il codice tariffa O la particolarità consegna                     5
064600100224Record già presente in tabella con le stesse caratteristiche anche annullato! 6
064700100224Già presente per il cliente un record con cod.tar o particolarità anche annull
064800100224Codice tariffa errato                                                         8
064900100224codice tariffa inesistente per il cliente                                     9
