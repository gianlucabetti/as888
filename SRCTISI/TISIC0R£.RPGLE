000100010628     H DECEDIT('0,') DATEDIT(*YMD.)
000200100210      ***************************************************************************
000300100224      * Tabella DCR
000400100210      *  clienti con calcolo in immissione bolle della data consegna richiesta
000500100210      ***************************************************************************
000600010628      *  RIEPILOGO INDICATORI
000700010628      ****************************************************************
000800010628      * 01 - RECORD ANNULLATO
000900010628      * 02 - *ON VARIAZIONE - *OFF IMMISSIONE
001000010628      * 28 - ERRORE GENERICO DSPF
001100010628      * 31 - GENERICO
001200010628      * 90 - RIEMISSIONE VIDEATA
001300010628      ****************************************************************
001400100224     Ftntam01L  IF   E           K DISK
001500100224     FTNTBE01L  IF   E           K DISK
001600100210     F                                     INFDS(tbe01)
001700100210     FTNTBE00f  UF A E             DISK    rename(tntbe000:tntbe0fis)
001800100210     Fcnaco00f  IF   E           K DISK
001900100210     FTNTB80D   CF   E             WORKSTN
002000100212     F                                        SFILE(tntb80s2:sflNRR)
002100010628      *------------------------------------------------------------------------*
002200100210     D TBE01           DS
002300100210     D  tbeNRR               397    400B 0
002400100210     D KCOD            S                   LIKE(TBECOD) inz('DCR')
002500010628     D KKE1            S                   LIKE(TBEKE1)
002600010628      *   S C H I E R E
002700100224     D MSG             S             78    DIM(9) CTDATA PERRCD(1)              MSG VIDEO
002800100210
002900100210     D ctrpa           S              3    DIM(15)
003000100210     D ore             S              4    DIM(15)
003100100211     D nrr             S              5  0 DIM(15)
003200100210     D*
003300010628     D TIBS02DS      E DS
003400100211     D DDCR          E DS
003500100210      *
003600100210     d TIBS34ds      e ds
003700100210     d dDatiUte      e ds
003800100210     d AZUTEds       e ds                  extname(AZUTE00F)
003900100210      *
004000010628     D*
004100010628     D KPJBA         E DS
004200100210     d
004300100210     D PARAM4          DS
004400100210     D  PA4COD                 6     12  0
004500100210     D  PA4CTR                13     15
004600100210     D  PA4FLG                16     16
004700100210     D  PA4VPR                17     17
004800100210     D  PA4CCC                18     21  0
004900100210     D  PA4DCV                22     36
005000100210     d  pa4prg                37     39
005100100210     d  pa4dtr                40     47
005200010628     D*
005300100210     d pardut          s             30
005400100210     d parrag          s             48
005500100210     d parkcc          s              4  0
005600100210     d parsta          s              1  0
005700100210     d parflr          s             90
005800100210     d pardit          s              3
005900100210     d parnum          s              2  0
006000100210     d parkcm          s             80
006100100210     d parksm          s            140
006200100210     d parkdm          s             60
006300100210     d paresci         s              1
006400100210     d parerr          s              2
006500100210     d pariva          s             16
006600100210     d parcdf          s             16
006700100210     d
006800100210     d kksc            s                   like(acoksc)
006900100224     d kctr            s                   like(tamctr)
007000100210     d xx              s              2  0
007100100210     d codut           s              1  0 inz(1)
007200100211     d wrem            s              3  0
007300100212     d sflnrr          s              4  0
007400100210     d
007500100210     d Datasys         s               d   inz(*sys) datfmt(*iso)
007600100210       // -?Costante per controllo "caratteri solo numerici"?
007700100210     d c_Digits        c                   const('0123456789')
007800010628     D*------------------------------------------------------------------------*
007900010628     C* Pulizia 1° Videata
008000100210     c     inizio        tag
008100100210     C                   MOVE      *BLANKS       V1Cksc
008200100210     C                   MOVE      '?'           V1Cksc
008300100210     C                   MOVE      *BLANKS       V1dksc
008400100224     c                   clear                   sflnrr
008500010628     C*
008600010628      * Emissione 1° Videata
008700100210     c     FOr01         tag
008800100210     C                   EXFMT     TNTB801
008900010628      * F3=Fine
009000100210     C   KC              goto      fine
009100010628     C*
009200010628      * Controlli 1° videata
009300100210     C                   SETOFF                                       2890
009400010628     C                   EXSR      SR_CONTR01
009500010628     C*
009600100210     C   90              goto      for01
009700010628     C*
009800010628      * Caricamento e pulizia seconda videata
009900010628     C                   EXSR      SR_CARVID
010000100210     c
010100100211     c* Emetto sfl se trovato almeno un record
010200100211     c                   if        xx>0
010300100211     C     for02         TAG
010400100212     c                   write     tntb80z2
010500100211     C                   EXFMT     TNTB80c2
010600100211     C*
010700100211     c                   setoff                                       9028
010800100211     C*
010900100211      * F3=Fine
011000100211     C   KC              goto      fine
011100100211     C*
011200100211      * F12=Ritorno
011300100211     C   KL              goto      for01
011400100211     c
011500100211     c* se effettuata scelta, o "F10-Nuova Immissione" -->  emetto video 3
011600100212     c                   readc     tntb80s2
011700100212
011800100212     c
011900100212     c                   if        v2csce='1'  or *inkj=*on
012000100211     c                   exsr      Caricavid3
012100100211     c                   else
012200100211     c                   goto      for02
012300100211     c                   endif
012400100211     c
012500100211     c                   endif
012600010628     C*
012700100210      * Emissione 3° Videata
012800100210     C     for03         TAG
012900100210     C                   EXFMT     TNTB803
013000010628     C*
013100100210     c                   setoff                                       9028
013200100212     c                   setoff                                       404142
013300010628     C*
013400010628      * F3=Fine
013500100210     C   KC              goto      fine
013600010628     C*
013700010628      * F12=Ritorno
013800100224     c                   if        *inkl
013900100224     c                   if        sflnrr=0
014000100224     C                   goto      for01
014100100224     c                   else
014200100224     C                   goto      for02
014300100224     c                   endif
014400100224     c                   endif
014500010628     C*
014600100210     C                   EXSR      SR_CONTR03
014700100210     C   90              GOTO      for03
014800010628     C*
014900010628      * F6=Aggiornamento o F5=Ripristino o F16=Annullamento
015000010628     C                   IF        *INKE = *ON OR *INKF = *ON OR *INKQ = *ON
015100010629     C                   EXSR      SR_REGISTRA
015200100210     C                   goto      inizio
015300010628     C                   ENDIF
015400010628     C*
015500100210     C                   GOTO      for03
015600040625     c*
015700100210     c     fine          tag
015800040625     c                   clear                   tibs02ds
015900040625     c                   movel     'C'           t02tla
016000040625     c                   call      'TIBS02R'
016100040625     c                   parm                    kpjba
016200040625     c                   parm                    tibs02ds
016300010628     C*
016400010628     C                   EVAL      *INLR = *ON
016500010628      **********************************************************************
016600010628      * CONTROLLI VIDEO 1
016700010628      **********************************************************************
016800010628     C     SR_CONTR01    BEGSR
016900010628      * Reset indicatori
017000100210     C     '?'           SCAN      V1Cksc                                 31
017100010628     C     *IN31         IFEQ      *ON
017200010628     C*
017300010628     C                   EVAL      *IN90 = *ON
017400010628     C                   CLEAR                   TIBS02DS
017500010628     C                   MOVEL     'R'           T02MOD
017600010628     C                   MOVEL     KNSIF         T02SIF
017700100210     C                   MOVEL     'DCR'         T02COD
017800010628     C                   CALL      'TIBS02R'
017900010628     C                   PARM                    KPJBA
018000010628     C                   PARM                    TIBS02DS
018100010628     C                   IF        T02ERR = *BLANKS
018200100210     C                   MOVEL     T02KE1        V1Cksc
018300040625     c                   else
018400100210     c                   clear                   v1cksc
018500010628     C                   SETON                                        90
018600100210     C                   GOTO      ENDSRCONTR
018700010628     C                   ENDIF
018800010628     C                   ENDIF
018900010628
019000100210     C* Verifico se richiesta la ricerca alfabetica
019100100210     c                   if        v1dKSC<>*blanks and v1cksc<=*zeros
019200100210     c                   eval      pardut=rsut
019300100210     c                   clear                   parrag
019400100210     c                   eval      parrag=v1dKSC
019500100210     c                   eval      parkcc=dutkci
019600100210     c                   eval      parsta=*zeros
019700100210     c                   clear                   parflr
019800100210     c                   clear                   pardit
019900100210     c                   eval      parnum=1
020000100210     c                   clear                   parkcm
020100100210     c                   clear                   parksm
020200100210     c                   clear                   parkdm
020300100210     c                   clear                   paresci
020400100210     c                   clear                   parerr
020500100210     c                   clear                   pariva
020600100210     c                   clear                   parcdf
020700100210     c                   call      'XALFA3BR'
020800100210     C                   PARM                    PARDUT
020900100210     C                   PARM                    codut
021000100210     C                   PARM                    parrag
021100100210     C                   PARM                    parkcc
021200100210     C                   PARM                    PARSTA
021300100210     C                   PARM                    PARFLR           90
021400100210     C                   PARM                    PARDIT            3
021500100210     C                   PARM                    PARNUM
021600100210     C                   PARM                    PARKCM
021700100210     C                   PARM                    PARKSM
021800100210     C                   PARM                    PARKDM
021900100210     C                   PARM                    PAResci
022000100210     C                   PARM                    PARerr
022100100210     C                   PARM                    PARiva
022200100210     C                   PARM                    PARcdf
022300100210     c
022400100210     c                   if        parsta<>-1 and parerr=*blanks
022500100210     c                   eval      v1cksc=parksm
022600100210     c                   eval      v1dksc=parrag
022700100210     c                   endif
022800100210
022900100210     c                   seton                                        90
023000100210     C                   GOTO      ENDSRCONTR
023100100210     c                   endif
023200100210     c
023300100210      * Controllo se campo valorizzato
023400100210     C                   IF        V1Cksc<= *ZEROS
023500100224     C                   SETON                                        2890
023600010628     C                   MOVEL     MSG(1)        V1CMSG
023700010628     C                   GOTO      ENDSRCONTR
023800010628     C                   ENDIF
023900100210     c
024000100210     c* controllo ksc
024100100210     c     c_digits      check     v1cksc                                 30
024200100211     c                   if        *in30
024300100210     C                   SETON                                        2890
024400100210     C                   MOVEL     MSG(2)        V1CMSG
024500100210     C                   GOTO      ENDSRCONTR
024600100210     C                   ENDIF
024700100210     c
024800100210     c                   movel     v1cksc        kksc
024900100210     c     kaco          chain     cnaco00f
025000100210     c                   if        not %found(cnaco00f) or acoabl<>*blanks
025100100210     C                   SETON                                        2890
025200100210     C                   MOVEL     MSG(3)        V1CMSG
025300100210     C                   GOTO      ENDSRCONTR
025400100210     C                   ENDIF
025500010628     C*
025600010628     C     ENDSRCONTR    endsr
025700010628      **********************************************************************
025800100210      * CARICAMENTO dati del cliente
025900010628      **********************************************************************
026000010628     C     SR_CARVID     BEGSR
026100100210     c                   setoff                                       0102
026200100211     c
026300100211     C                   SETON                                        35
026400100211     C                   WRITE     tntb80c2
026500100211     C                   SETOFF                                       35
026600100210     c
026700100210     c* Leggo tutti i codici che trovo per ogni codice cliente
026800100212     c                   clear                   sflnrr
026900100212     c                   clear                   xx
027000100210     c                   clear                   ctrpa
027100100210     c                   clear                   ore
027200100212     c                   clear                   v2hnrr
027300100211     c                   clear                   v3cctr
027400100211     c                   clear                   v3ctcr
027500100211     c                   clear                   v3cpart
027600100211     c                   clear                   v3core
027700040625     c*
027800100210     C                   MOVEL(p)  V1Cksc        KKE1
027900100210     C                   MOVEL(p)  V1Cksc        kksc
028000100210     c
028100100210     C     Ktbe          setll     TNTBE01L
028200100210     C     ktbe          reade     TNTBE01L
028300100212     c
028400100210     c                   dow       not %eof(tntbe01l)
028500100210     c
028600100210     c                   eval      xx=xx+1
028700100210     c                   eval      ctrpa(xx)=%subst(tbeke2:1:3)
028800100210     c                   eval      ore(xx)=%subst(tbeuni:1:4)
028900100211     c                   eval      nrr(xx)=tbenrr
029000100211     c                   exsr      Caricasfl
029100100210     c
029200100210     C     ktbe          reade     TNTBE01L
029300100210     c                   enddo
029400010628     C*
029500100212     c                   clear                   v2cdes
029600100212     c                   clear                   v2cctr
029700100210     c                   clear                   v2cpart
029800100210     c                   clear                   v2ctcr
029900100210     c                   clear                   v2core
030000100210     c                   movel     acorag        v2dksc
030100100210     c
030200100210     c                   if        xx=0
030300100212     C                   MOVEL     'IMMISSIONE'  V3vAIM
030400100210     c                   seton                                        01
030500100211     c                   endif
030600010628     C*
030700010628     C                   ENDSR
030800100211      **********************************************************************
030900100211      * Carica SFL
031000100211      **********************************************************************
031100100211     C     Caricasfl     BEGSR
031200100211     c
031300100212     c                   clear                   v2cpart
031400100212     c                   clear                   v2cctr
031500100212     c                   select
031600100212     c                   when      ctrpa(xx)='TTR'
031700100212     c                   eval      v2cpart='SI'
031800100212     c                   eval      v2cdes='Per Particolarità CONSEGNA'
031900100212
032000100212     c                   when      ctrpa(xx)>=*zeros
032100100212     c                   eval      v2cctr=ctrpa(xx)
032200100212     c                   eval      v2cdes='Per Codice Tariffa: '+ v2cctr
032300100212     c                   other
032400100212
032500100212     c                   eval      v2cdes='Per tutte le bolle'
032600100212     c                   endsl
032700100211
032800100211     c                   movel     ore(xx)       v2ctcr
032900100211     c                   move      ore(xx)       v2core
033000100212     c                   move      nrr(xx)       v2hnrr
033100100211     c                   select
033200100211     c                   when      v2ctcr=' '
033300100211     c                   eval      v2dtcr='   IL    '
033400100211     c                   when      v2ctcr='D'
033500100211     c                   eval      v2dtcr=' DOPO IL '
033600100211     c                   when      v2ctcr='P'
033700100211     c                   eval      v2dtcr='PRIMA DEL'
033800100211     c                   endsl
033900100212     c
034000100212     c                   if        tbeatb<>' '
034100100212     c                   eval      v2cann='ANNULLATO'
034200100212     c                   seton                                        04
034300100212     c                   else
034400100212     c                   clear                   v2cann
034500100212     c                   setoff                                       04
034600100212     c                   endif
034700100211     c
034800100212     c                   eval      sflnrr=sflnrr+1
034900100211     c                   write     tntb80s2
035000100211     c
035100100211     C                   ENDSR
035200100211      **********************************************************************
035300100211      * carica dati record scelto nella videata singola
035400100211      **********************************************************************
035500100211     c     Caricavid3    BEGSR
035600100224     c                   if        not *inkj and v2csce='1'
035700100212     c                   clear                   v2csce
035800100212     c                   if        v2cann<>*blanks
035900100212     c                   seton                                        04
036000100212     c                   else
036100100212     c                   setoff                                       04
036200100212     c                   endif
036300100212     c                   update    tntb80s2
036400100224     c                   endif
036500100211     c
036600100211     c                   if        *inkj=*on
036700100211     c                   clear                   v3cctr
036800100211     c                   clear                   v3cpart
036900100211     c                   clear                   v3core
037000100211     c                   clear                   v3ctcr
037100100212     c                   clear                   v2hnrr
037200100211     c                   seton                                        01
037300100212     c                   setoff                                       0203
037400100211     C                   MOVEL     'IMMISSIONE'  V3VAIM
037500100211     c                   else
037600100211     c
037700100212     c     sflnrr        chain     tntb80s2
037800100211     c
037900100211     c                   if        %found
038000100211     c*  Se presente solo uno emetto videata di modifica altrimenti di scelta
038100100211     c                   seton                                        02
038200100212     c                   setoff                                       0103
038300100212     c                   if        v2cann=*blanks
038400100211     C                   MOVEL     'VARIAZIONE'  V3VAIM
038500100212     c                   else
038600100212     C                   MOVEL     'ANNULLATO '  V3VAIM
038700100212     c                   seton                                        03
038800100212     c                   endif
038900100211     c
039000100211     c                   eval      v3cctr =v2cctr
039100100211     c                   eval      v3cpart=v2cpart
039200100211     c                   eval      v3core =v2core
039300100211     c                   eval      v3ctcr =v2ctcr
039400100211     c                   endif
039500100212     c                   endif
039600100211     c
039700100211     C                   ENDSR
039800010628      **********************************************************************
039900010628      * CONTROLLI VIDEO 2
040000010628      **********************************************************************
040100100210     C     Sr_Contr03    begsr
040200100210     c
040300100210     c* Ricerca col '?' sul codice tariffa
040400100212     C     '?'           SCAN      V3Cctr                                 31
040500100210     c
040600100210     C     *IN31         IFEQ      *ON
040700100210     C                   CLEAR                   PARAM4
040800100210     C                   MOVEL     v1cksc        PA4COD
040900100210     C                   Z-ADD     dutkci        PA4CCC
041000100210     c                   movel     '2'           pa4vpr
041100100210     c                   move      dateu         pa4dtr
041200100210     C                   MOVEL     PARAM4        KPJBU
041300100210     C                   CALL      'TNTA36R'
041400100210     C                   PARM                    KPJBA
041500100210     C                   MOVEL     KPJBU         PARAM4
041600100210     C*
041700100210    2C     PA4CTR        IFNE      *BLANKS
041800100212     c                   movel     pa4ctr        v3cctr
041900100211     c                   seton                                        9040
042000100211     c                   goto      endct3
042100100210     c                   endif
042200100210     c                   endif
042300100224     c
042400100224     c* Se immesso codice tariffa, verifico sse esiste aleno un record (non mi interessa
042500100224     c*  se scaduto, decorrente o ancora da decorrere...)
042600100224     c                   if        v3cCtr<>*blanks
042700100224     c                   if        v3cctr<*zeros
042800100224     C                   MOVEL     MSG(8)        V1CMSG
042900100224     c                   seton                                        904028
043000100224     c                   goto      endct3
043100100224     c                   endif
043200100224     c
043300100224     c     c_digits      check     v3cctr                                 30
043400100224     c                   if        *in30
043500100224     C                   MOVEL     MSG(8)        V1CMSG
043600100224     c                   seton                                        904028
043700100224     c                   goto      endct3
043800100224     c                   endif
043900100224     c
044000100224     c                   movel     v3cctr        kctr
044100100224     c     ktam          setll     tntam01l
044200100224     c     ktam          reade     tntam01l
044300100224     c                   dow       not %eof(tntam01l) and tamatb<>' '
044400100224     c     ktam          reade     tntam01l
044500100224     c                   enddo
044600100224     c
044700100224     c                   if        %eof(tntam01l)
044800100224     C                   MOVEL     MSG(9)        V1CMSG
044900100224     c                   seton                                        904028
045000100224     c                   goto      endct3
045100100224     c                   endif
045200100224     c                   endif
045300100211     c
045400100211     c* Cod tariffa e particolarità sono in alternativa
045500100212     c                   if        v3cctr<>*blanks and v3cpart<>'  '
045600100211     C                   MOVEL     MSG(5)        V1CMSG
045700100212     c                   seton                                        904128
045800100211     c                   goto      endct3
045900100211     c                   endif
046000100212     c
046100100212     c* Verifico se per quel cliente esiste già un altro record
046200100212     c                   z-add     1             xx
046300100212    1c                   dow       nrr(xx)>0
046400100212    2c                   if        nrr(xx)<>v2hnrr
046500100212     c*
046600100212    3c                   select
046700100212     c                   when      ctrpa(xx)=*blanks and v3cpart='  '
046800100212     c                             and v3cctr=*blanks
046900100212     C                   MOVEL     MSG(6)        V1CMSG
047000100212     c                   seton                                        904128
047100100212     c                   goto      endct3
047200100212     c
047300100212     c                   when      (ctrpa(xx)='TTR' and v3cpart='SI')  or
047400100212     c                             (ctrpa(xx)>=*zeros and ctrpa(xx)=v3cctr)
047500100212     C                   MOVEL     MSG(6)        V1CMSG
047600100212     c                   seton                                        904128
047700100212     c                   goto      endct3
047800100212
047900100212     c                   when      ctrpa(xx)<>*blanks and (v3cpart='SI'  or
048000100212     C                              V3CCTR<>*BLANKS)
048100100212     C                   MOVEL     MSG(7)        V1CMSG
048200100212     c                   seton                                        904128
048300100212     c                   goto      endct3
048400100212    3c                   endsl
048500100212    2c                   endif
048600100212     c                   eval      xx=xx+1
048700100212    1c                   enddo
048800100211     c
048900100211     C* ORE: obbbligatorie e multiplo di 12
049000100212     c                   if        v3core=0
049100100211     C                   MOVEL     MSG(4)        V1CMSG
049200100212     c                   seton                                        904228
049300100211     c                   goto      endct3
049400100211     c                   endif
049500100211     c
049600100211     c                   eval      wrem=%rem(v3core:12)
049700100211     c                   if        wrem>0
049800100211     C                   MOVEL     MSG(4)        V1CMSG
049900100212     c                   seton                                        904228
050000100211     c                   goto      endct3
050100100211     c                   endif
050200100211     c
050300100212     c
050400100211     C     ENDCT3        ENDSR
050500010628      **********************************************************************
050600010628      * REGISTRAZIONE DATI IMMESSI
050700010628      **********************************************************************
050800010628     C     SR_REGISTRA   BEGSR
050900100211     c
051000100212     c* record nuovo
051100100211     c                   if        *in01
051200100211     c                   clear                   tntbe0fis
051300100211     c                   eval      tbecod='DCR'
051400100211     c                   eval      tbeapl='OP'
051500100211     c                   else
051600100211     c
051700100211     c* Se record esistente, chaino il fisico
051800100212     c     v2hnrr        chain     tntbe00f
051900100212     c                   endif
052000100211     c
052100100211     c                   eval      tbeke1=v1cksc
052200100211
052300100211     c                   clear                   tbeke2
052400100211     c                   select
052500100211     c                   when      v3cctr<>*blanks
052600100212     c                   eval      tbeke2=v3cctr
052700100211     c                   when      v3cpart<>*blanks
052800100211     c                   eval      tbeke2='TTR'
052900100211     c                   endsl
053000100211     c
053100100211     c                   clear                   ddcr
053200100211     c                   eval      §dcrtcr=v3ctcr
053300100211     c                   eval      §dcrore=v3core
053400100211     c                   movel(p)  ddcr          tbeuni
053500100211     c
053600100211     c                   clear                   tbeftr
053700100211     c                   eval      tbeftt='S'
053800100212     c* Annullamento
053900100224     c                   if        *inkq=*on
054000100212     c                   eval      tbeatb='A'
054100100212     c                   endif
054200100212     c* Ripristino
054300100212     c                   if        *inke=*on
054400100212     c                   eval      tbeatb=' '
054500100212     c                   endif
054600100211     c
054700100211     c                   if        *in01
054800100211     c                   write     tntbe0fis
054900100211     c                   else
055000100211     c                   update    tntbe0fis
055100100211     c                   endif
055200010628
055300040625     C                   ENDSR
055400010628      *****************************************************************
055500010628      * ROUTINE INIZIALE
055600010628      *****************************************************************
055700010628     C     *INZSR        BEGSR
055800010628      *
055900010628     C     *ENTRY        PLIST
056000010628     C                   PARM                    KPJBA
056100100210      *
056200100210      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
056300100210     c     *dtaara       define    §azute        azuteds
056400100210     c     *dtaara       define    §datiute      ddatiute
056500100210      *
056600100210     c                   clear                   AzUteDs
056700100210     c                   clear                   DDatiUte
056800100210     c                   clear                   Tibs34Ds
056900100210     c                   in(E)     *dtaara
057000100210if  1c                   if        %Error  or  RSUT = *blanks
057100100210     c                   call      'TIBS34R'
057200100210     c                   parm                    Tibs34Ds
057300100210     c                   in        *dtaara
057400100210e   1c                   endif
057500100210     c
057600100210     c     *iso          movel     datasys       dateu             8 0
057700010628     C*
057800100210     C     Ktbe          KLIST
057900010628     C                   KFLD                    KCOD
058000010628     C                   KFLD                    KKE1
058100100210     C     Kaco          KLIST
058200100210     C                   KFLD                    codut
058300100210     C                   KFLD                    dutkci
058400100210     C                   KFLD                    kksc
058500100224     C     KTAM          KLIST
058600100224     C                   KFLD                    kksc
058700100224     C                   KFLD                    kctr
058800010628     C*
058900010628     C                   endsr
059000010628      *---------------------------------------------------------------------------------------------
059100010628** MSG  Lungh. 78                                                            *
059200100210Codice Cliente obbligatorio                                                   1
059300100211Codice Cliente errato                                                         2
059400100211Codice Cliente non trovato nel piano dei conti o bloccato                     3
059500100212Numero di ore obbligatorio: deve essere un multiplo di 12                     4
059600100211Immettere O il codice tariffa O la particolarità consegna                     5
059700100224Record già presente in tabella con le stesse caratteristiche anche annullato! 6
059800100224Già presente per il cliente un record con cod.tar o particolarità anche annull
059900100224Codice tariffa errato                                                         8
060000100224codice tariffa inesistente per il cliente                                     9
