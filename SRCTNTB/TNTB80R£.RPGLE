000100110801     /*END
000200010628     H DECEDIT('0,') DATEDIT(*YMD.)
000300100210      ***************************************************************************
000400100224      * Tabella DCR
000500100210      *  clienti con calcolo in immissione bolle della data consegna richiesta
000600100210      ***************************************************************************
000700010628      *  RIEPILOGO INDICATORI
000800010628      ****************************************************************
000900010628      * 01 - RECORD ANNULLATO
001000010628      * 02 - *ON VARIAZIONE - *OFF IMMISSIONE
001100010628      * 28 - ERRORE GENERICO DSPF
001200010628      * 31 - GENERICO
001300010628      * 90 - RIEMISSIONE VIDEATA
001400010628      ****************************************************************
001500100224     Ftntam01L  IF   E           K DISK
001600100224     FTNTBE01L  IF   E           K DISK
001700100210     F                                     INFDS(tbe01)
001800100210     FTNTBE00f  UF A E             DISK    rename(tntbe000:tntbe0fis)
001900100210     Fcnaco00f  IF   E           K DISK
002000100210     FTNTB80D   CF   E             WORKSTN
002100100212     F                                        SFILE(tntb80s2:sflNRR)
002200010628      *------------------------------------------------------------------------*
002300100210     D TBE01           DS
002400100210     D  tbeNRR               397    400B 0
002500100210     D KCOD            S                   LIKE(TBECOD) inz('DCR')
002600010628     D KKE1            S                   LIKE(TBEKE1)
002700010628      *   S C H I E R E
002800100224     D MSG             S             78    DIM(9) CTDATA PERRCD(1)              MSG VIDEO
002900100210
003000100210     D ctrpa           S              3    DIM(15)
003100100210     D ore             S              4    DIM(15)
003200100211     D nrr             S              5  0 DIM(15)
003300100210     D*
003400010628     D TIBS02DS      E DS
003500100211     D DDCR          E DS
003600100210      *
003700100210     d TIBS34ds      e ds
003800100210     d dDatiUte      e ds
003900100210     d AZUTEds       e ds                  extname(AZUTE00F)
004000100210      *
004100010628     D*
004200010628     D KPJBA         E DS
004300110802     d TNTA61ds      e ds
004400110802     d*
004500100210     D PARAM4          DS
004600100210     D  PA4COD                 6     12  0
004700100210     D  PA4CTR                13     15
004800100210     D  PA4FLG                16     16
004900100210     D  PA4VPR                17     17
005000100210     D  PA4CCC                18     21  0
005100100210     D  PA4DCV                22     36
005200100210     d  pa4prg                37     39
005300100210     d  pa4dtr                40     47
005400010628     D*
005500100210     d pardut          s             30
005600100210     d parrag          s             48
005700100210     d parkcc          s              4  0
005800100210     d parsta          s              1  0
005900100210     d parflr          s             90
006000100210     d pardit          s              3
006100100210     d parnum          s              2  0
006200100210     d parkcm          s             80
006300100210     d parksm          s            140
006400100210     d parkdm          s             60
006500100210     d paresci         s              1
006600100210     d parerr          s              2
006700100210     d pariva          s             16
006800100210     d parcdf          s             16
006900100210     d
007000100210     d kksc            s                   like(acoksc)
007100100224     d kctr            s                   like(tamctr)
007200100210     d xx              s              2  0
007300100210     d codut           s              1  0 inz(1)
007400100211     d wrem            s              3  0
007500100212     d sflnrr          s              4  0
007600100210     d
007700100210     d Datasys         s               d   inz(*sys) datfmt(*iso)
007800110802       // -?Costante per controllo "caratteri solo numerici"?
007900110802     d c_Digits        c                   const('0123456789')
008000010628     D*------------------------------------------------------------------------*
008100110801     c                   eval      *in10 = *on
008200110802     c                   Select
008300110802     c                   When      %parms() > 1  and  TA61ksc <= *zero
008400110802     c                   eval      TA61err = *on
008500110802     c                   eval      TA61msg = 'Richiesto il codice cliente'
008600110802     c                   goto      Fine
008700110802     c                   When      %parms() > 1
008800110802     c                   eval      V1Cksc  = %editc(TA61ksc:'X')
008900110801     c                   eval      *in10 = *off
009000110801     c                   setoff                                       2890
009100110801     c                   exsr      sr_Contr01
009200110801     c   90              goto      Fine
009300110801     c  N90              goto      Car02
009400110802     c                   EndSl
009500010628     C* Pulizia 1° Videata
009600100210     c     inizio        tag
009700100210     C                   MOVE      *BLANKS       V1Cksc
009800100210     C                   MOVE      '?'           V1Cksc
009900100210     C                   MOVE      *BLANKS       V1dksc
010000100224     c                   clear                   sflnrr
010100010628     C*
010200010628      * Emissione 1° Videata
010300100210     c     FOr01         tag
010400100210     C                   EXFMT     TNTB801
010500010628      * F3=Fine
010600100210     C   KC              goto      fine
010700010628     C*
010800010628      * Controlli 1° videata
010900100210     C                   SETOFF                                       2890
011000010628     C                   EXSR      SR_CONTR01
011100010628     C*
011200100210     C   90              goto      for01
011300010628     C*
011400110801     c     Car02         tag
011500110801      *
011600010628      * Caricamento e pulizia seconda videata
011700010628     C                   EXSR      SR_CARVID
011800110801     c*
011900100211     c* Emetto sfl se trovato almeno un record
012000100211     c                   if        xx>0
012100100211     C     for02         TAG
012200100212     c                   write     tntb80z2
012300100211     C                   EXFMT     TNTB80c2
012400100211     C*
012500100211     c                   setoff                                       9028
012600100211     C*
012700100211      * F3=Fine
012800100211     C   KC              goto      fine
012900100211     C*
013000100211      * F12=Ritorno
013100110801     C*//KL              goto      for01
013200110801     c   KL              if        Not  *in10
013300110801     c                   goto      For01
013400110801     c                   else
013500110801     c                   goto      Fine
013600110801     c                   endif
013700110801     c*
013800100211     c* se effettuata scelta, o "F10-Nuova Immissione" -->  emetto video 3
013900100212     c                   readc     tntb80s2
014000110802      *
014100100212     c
014200100212     c                   if        v2csce='1'  or *inkj=*on
014300100211     c                   exsr      Caricavid3
014400100211     c                   else
014500100211     c                   goto      for02
014600100211     c                   endif
014700100211     c
014800100211     c                   endif
014900010628     C*
015000100210      * Emissione 3° Videata
015100100210     C     for03         TAG
015200100210     C                   EXFMT     TNTB803
015300010628     C*
015400100210     c                   setoff                                       9028
015500100212     c                   setoff                                       404142
015600010628     C*
015700010628      * F3=Fine
015800100210     C   KC              goto      fine
015900010628     C*
016000010628      * F12=Ritorno
016100100224     c                   if        *inkl
016200100224     c                   if        sflnrr=0
016300100224     C                   goto      for01
016400100224     c                   else
016500100224     C                   goto      for02
016600100224     c                   endif
016700100224     c                   endif
016800010628     C*
016900100210     C                   EXSR      SR_CONTR03
017000100210     C   90              GOTO      for03
017100010628     C*
017200010628      * F6=Aggiornamento o F5=Ripristino o F16=Annullamento
017300010628     C                   IF        *INKE = *ON OR *INKF = *ON OR *INKQ = *ON
017400010629     C                   EXSR      SR_REGISTRA
017500100210     C                   goto      inizio
017600010628     C                   ENDIF
017700010628     C*
017800100210     C                   GOTO      for03
017900040625     c*
018000100210     c     fine          tag
018100040625     c                   clear                   tibs02ds
018200040625     c                   movel     'C'           t02tla
018300040625     c                   call      'TIBS02R'
018400040625     c                   parm                    kpjba
018500040625     c                   parm                    tibs02ds
018600010628     C*
018700010628     C                   EVAL      *INLR = *ON
018800010628      **********************************************************************
018900010628      * CONTROLLI VIDEO 1
019000010628      **********************************************************************
019100010628     C     SR_CONTR01    BEGSR
019200010628      * Reset indicatori
019300100210     C     '?'           SCAN      V1Cksc                                 31
019400010628     C     *IN31         IFEQ      *ON
019500010628     C*
019600010628     C                   EVAL      *IN90 = *ON
019700010628     C                   CLEAR                   TIBS02DS
019800010628     C                   MOVEL     'R'           T02MOD
019900010628     C                   MOVEL     KNSIF         T02SIF
020000100210     C                   MOVEL     'DCR'         T02COD
020100010628     C                   CALL      'TIBS02R'
020200010628     C                   PARM                    KPJBA
020300010628     C                   PARM                    TIBS02DS
020400010628     C                   IF        T02ERR = *BLANKS
020500100210     C                   MOVEL     T02KE1        V1Cksc
020600040625     c                   else
020700100210     c                   clear                   v1cksc
020800010628     C                   SETON                                        90
020900100210     C                   GOTO      ENDSRCONTR
021000010628     C                   ENDIF
021100010628     C                   ENDIF
021200010628
021300100210     C* Verifico se richiesta la ricerca alfabetica
021400100210     c                   if        v1dKSC<>*blanks and v1cksc<=*zeros
021500100210     c                   eval      pardut=rsut
021600100210     c                   clear                   parrag
021700100210     c                   eval      parrag=v1dKSC
021800100210     c                   eval      parkcc=dutkci
021900100210     c                   eval      parsta=*zeros
022000100210     c                   clear                   parflr
022100100210     c                   clear                   pardit
022200100210     c                   eval      parnum=1
022300100210     c                   clear                   parkcm
022400100210     c                   clear                   parksm
022500100210     c                   clear                   parkdm
022600100210     c                   clear                   paresci
022700100210     c                   clear                   parerr
022800100210     c                   clear                   pariva
022900100210     c                   clear                   parcdf
023000100210     c                   call      'XALFA3BR'
023100100210     C                   PARM                    PARDUT
023200100210     C                   PARM                    codut
023300100210     C                   PARM                    parrag
023400100210     C                   PARM                    parkcc
023500100210     C                   PARM                    PARSTA
023600100210     C                   PARM                    PARFLR           90
023700100210     C                   PARM                    PARDIT            3
023800100210     C                   PARM                    PARNUM
023900100210     C                   PARM                    PARKCM
024000100210     C                   PARM                    PARKSM
024100100210     C                   PARM                    PARKDM
024200100210     C                   PARM                    PAResci
024300100210     C                   PARM                    PARerr
024400100210     C                   PARM                    PARiva
024500100210     C                   PARM                    PARcdf
024600100210     c
024700100210     c                   if        parsta<>-1 and parerr=*blanks
024800100210     c                   eval      v1cksc=parksm
024900100210     c                   eval      v1dksc=parrag
025000100210     c                   endif
025100100210
025200100210     c                   seton                                        90
025300100210     C                   GOTO      ENDSRCONTR
025400100210     c                   endif
025500100210     c
025600100210      * Controllo se campo valorizzato
025700100210     C                   IF        V1Cksc<= *ZEROS
025800100224     C                   SETON                                        2890
025900010628     C                   MOVEL     MSG(1)        V1CMSG
026000010628     C                   GOTO      ENDSRCONTR
026100010628     C                   ENDIF
026200100210     c
026300100210     c* controllo ksc
026400100210     c     c_digits      check     v1cksc                                 30
026500100211     c                   if        *in30
026600100210     C                   SETON                                        2890
026700100210     C                   MOVEL     MSG(2)        V1CMSG
026800100210     C                   GOTO      ENDSRCONTR
026900100210     C                   ENDIF
027000100210     c
027100100210     c                   movel     v1cksc        kksc
027200100210     c     kaco          chain     cnaco00f
027300100210     c                   if        not %found(cnaco00f) or acoabl<>*blanks
027400100210     C                   SETON                                        2890
027500100210     C                   MOVEL     MSG(3)        V1CMSG
027600100210     C                   GOTO      ENDSRCONTR
027700100210     C                   ENDIF
027800010628     C*
027900010628     C     ENDSRCONTR    endsr
028000010628      **********************************************************************
028100100210      * CARICAMENTO dati del cliente
028200010628      **********************************************************************
028300010628     C     SR_CARVID     BEGSR
028400100210     c                   setoff                                       0102
028500100211     c
028600100211     C                   SETON                                        35
028700100211     C                   WRITE     tntb80c2
028800100211     C                   SETOFF                                       35
028900100210     c
029000100210     c* Leggo tutti i codici che trovo per ogni codice cliente
029100100212     c                   clear                   sflnrr
029200100212     c                   clear                   xx
029300100210     c                   clear                   ctrpa
029400100210     c                   clear                   ore
029500100212     c                   clear                   v2hnrr
029600100211     c                   clear                   v3cctr
029700100211     c                   clear                   v3ctcr
029800100211     c                   clear                   v3cpart
029900100211     c                   clear                   v3core
030000040625     c*
030100100210     C                   MOVEL(p)  V1Cksc        KKE1
030200100210     C                   MOVEL(p)  V1Cksc        kksc
030300100210     c
030400100210     C     Ktbe          setll     TNTBE01L
030500100210     C     ktbe          reade     TNTBE01L
030600100212     c
030700100210     c                   dow       not %eof(tntbe01l)
030800100210     c
030900100210     c                   eval      xx=xx+1
031000100210     c                   eval      ctrpa(xx)=%subst(tbeke2:1:3)
031100100210     c                   eval      ore(xx)=%subst(tbeuni:1:4)
031200100211     c                   eval      nrr(xx)=tbenrr
031300100211     c                   exsr      Caricasfl
031400100210     c
031500100210     C     ktbe          reade     TNTBE01L
031600100210     c                   enddo
031700010628     C*
031800100212     c                   clear                   v2cdes
031900100212     c                   clear                   v2cctr
032000100210     c                   clear                   v2cpart
032100100210     c                   clear                   v2ctcr
032200100210     c                   clear                   v2core
032300100210     c                   movel     acorag        v2dksc
032400100210     c
032500100210     c                   if        xx=0
032600100212     C                   MOVEL     'IMMISSIONE'  V3vAIM
032700100210     c                   seton                                        01
032800100211     c                   endif
032900010628     C*
033000010628     C                   ENDSR
033100100211      **********************************************************************
033200100211      * Carica SFL
033300100211      **********************************************************************
033400100211     C     Caricasfl     BEGSR
033500100211     c
033600100212     c                   clear                   v2cpart
033700100212     c                   clear                   v2cctr
033800100212     c                   select
033900100212     c                   when      ctrpa(xx)='TTR'
034000100212     c                   eval      v2cpart='SI'
034100100212     c                   eval      v2cdes='Per Particolarità CONSEGNA'
034200100212
034300100212     c                   when      ctrpa(xx)>=*zeros
034400100212     c                   eval      v2cctr=ctrpa(xx)
034500100212     c                   eval      v2cdes='Per Codice Tariffa: '+ v2cctr
034600100212     c                   other
034700100212
034800100212     c                   eval      v2cdes='Per tutte le bolle'
034900100212     c                   endsl
035000100211
035100100211     c                   movel     ore(xx)       v2ctcr
035200100211     c                   move      ore(xx)       v2core
035300100212     c                   move      nrr(xx)       v2hnrr
035400100211     c                   select
035500100211     c                   when      v2ctcr=' '
035600100211     c                   eval      v2dtcr='   IL    '
035700100211     c                   when      v2ctcr='D'
035800100211     c                   eval      v2dtcr=' DOPO IL '
035900100211     c                   when      v2ctcr='P'
036000100211     c                   eval      v2dtcr='PRIMA DEL'
036100100211     c                   endsl
036200100212     c
036300100212     c                   if        tbeatb<>' '
036400100212     c                   eval      v2cann='ANNULLATO'
036500100212     c                   seton                                        04
036600100212     c                   else
036700100212     c                   clear                   v2cann
036800100212     c                   setoff                                       04
036900100212     c                   endif
037000100211     c
037100100212     c                   eval      sflnrr=sflnrr+1
037200100211     c                   write     tntb80s2
037300100211     c
037400100211     C                   ENDSR
037500100211      **********************************************************************
037600100211      * carica dati record scelto nella videata singola
037700100211      **********************************************************************
037800100211     c     Caricavid3    BEGSR
037900100224     c                   if        not *inkj and v2csce='1'
038000100212     c                   clear                   v2csce
038100100212     c                   if        v2cann<>*blanks
038200100212     c                   seton                                        04
038300100212     c                   else
038400100212     c                   setoff                                       04
038500100212     c                   endif
038600100212     c                   update    tntb80s2
038700100224     c                   endif
038800100211     c
038900100211     c                   if        *inkj=*on
039000100211     c                   clear                   v3cctr
039100100211     c                   clear                   v3cpart
039200100211     c                   clear                   v3core
039300100211     c                   clear                   v3ctcr
039400100212     c                   clear                   v2hnrr
039500100211     c                   seton                                        01
039600100212     c                   setoff                                       0203
039700100211     C                   MOVEL     'IMMISSIONE'  V3VAIM
039800100211     c                   else
039900100211     c
040000100212     c     sflnrr        chain     tntb80s2
040100100211     c
040200100211     c                   if        %found
040300100211     c*  Se presente solo uno emetto videata di modifica altrimenti di scelta
040400100211     c                   seton                                        02
040500100212     c                   setoff                                       0103
040600100212     c                   if        v2cann=*blanks
040700100211     C                   MOVEL     'VARIAZIONE'  V3VAIM
040800100212     c                   else
040900100212     C                   MOVEL     'ANNULLATO '  V3VAIM
041000100212     c                   seton                                        03
041100100212     c                   endif
041200100211     c
041300100211     c                   eval      v3cctr =v2cctr
041400100211     c                   eval      v3cpart=v2cpart
041500100211     c                   eval      v3core =v2core
041600100211     c                   eval      v3ctcr =v2ctcr
041700100211     c                   endif
041800100212     c                   endif
041900100211     c
042000100211     C                   ENDSR
042100010628      **********************************************************************
042200010628      * CONTROLLI VIDEO 2
042300010628      **********************************************************************
042400100210     C     Sr_Contr03    begsr
042500100210     c
042600100210     c* Ricerca col '?' sul codice tariffa
042700100212     C     '?'           SCAN      V3Cctr                                 31
042800100210     c
042900100210     C     *IN31         IFEQ      *ON
043000100210     C                   CLEAR                   PARAM4
043100100210     C                   MOVEL     v1cksc        PA4COD
043200100210     C                   Z-ADD     dutkci        PA4CCC
043300100210     c                   movel     '2'           pa4vpr
043400100210     c                   move      dateu         pa4dtr
043500100210     C                   MOVEL     PARAM4        KPJBU
043600100210     C                   CALL      'TNTA36R'
043700100210     C                   PARM                    KPJBA
043800100210     C                   MOVEL     KPJBU         PARAM4
043900100210     C*
044000100210    2C     PA4CTR        IFNE      *BLANKS
044100100212     c                   movel     pa4ctr        v3cctr
044200100211     c                   seton                                        9040
044300100211     c                   goto      endct3
044400100210     c                   endif
044500100210     c                   endif
044600100224     c
044700100323     c* Se immesso codice tariffa, verifico se esiste almeno un record (non mi interessa
044800100224     c*  se scaduto, decorrente o ancora da decorrere...)
044900100224     c                   if        v3cCtr<>*blanks
045000100224     c                   if        v3cctr<*zeros
045100100224     C                   MOVEL     MSG(8)        V1CMSG
045200100224     c                   seton                                        904028
045300100224     c                   goto      endct3
045400100224     c                   endif
045500100224     c
045600100224     c     c_digits      check     v3cctr                                 30
045700100224     c                   if        *in30
045800100224     C                   MOVEL     MSG(8)        V1CMSG
045900100224     c                   seton                                        904028
046000100224     c                   goto      endct3
046100100224     c                   endif
046200100224     c
046300100224     c                   movel     v3cctr        kctr
046400100224     c     ktam          setll     tntam01l
046500100224     c     ktam          reade     tntam01l
046600100224     c                   dow       not %eof(tntam01l) and tamatb<>' '
046700100224     c     ktam          reade     tntam01l
046800100224     c                   enddo
046900100224     c
047000100224     c                   if        %eof(tntam01l)
047100100224     C                   MOVEL     MSG(9)        V1CMSG
047200100224     c                   seton                                        904028
047300100224     c                   goto      endct3
047400100224     c                   endif
047500100224     c                   endif
047600100211     c
047700100211     c* Cod tariffa e particolarità sono in alternativa
047800100212     c                   if        v3cctr<>*blanks and v3cpart<>'  '
047900100211     C                   MOVEL     MSG(5)        V1CMSG
048000100212     c                   seton                                        904128
048100100211     c                   goto      endct3
048200100211     c                   endif
048300100212     c
048400100212     c* Verifico se per quel cliente esiste già un altro record
048500100212     c                   z-add     1             xx
048600100212    1c                   dow       nrr(xx)>0
048700100212    2c                   if        nrr(xx)<>v2hnrr
048800100212     c*
048900100212    3c                   select
049000100212     c                   when      ctrpa(xx)=*blanks and v3cpart='  '
049100100212     c                             and v3cctr=*blanks
049200100212     C                   MOVEL     MSG(6)        V1CMSG
049300100212     c                   seton                                        904128
049400100212     c                   goto      endct3
049500100212     c
049600100212     c                   when      (ctrpa(xx)='TTR' and v3cpart='SI')  or
049700100212     c                             (ctrpa(xx)>=*zeros and ctrpa(xx)=v3cctr)
049800100212     C                   MOVEL     MSG(6)        V1CMSG
049900100212     c                   seton                                        904128
050000100212     c                   goto      endct3
050100100212
050200100212     c                   when      ctrpa(xx)<>*blanks and (v3cpart='SI'  or
050300100212     C                              V3CCTR<>*BLANKS)
050400100212     C                   MOVEL     MSG(7)        V1CMSG
050500100212     c                   seton                                        904128
050600100212     c                   goto      endct3
050700100212    3c                   endsl
050800100212    2c                   endif
050900100212     c                   eval      xx=xx+1
051000100212    1c                   enddo
051100100211     c
051200100211     C* ORE: obbbligatorie e multiplo di 12
051300100212     c                   if        v3core=0
051400100211     C                   MOVEL     MSG(4)        V1CMSG
051500100212     c                   seton                                        904228
051600100211     c                   goto      endct3
051700100211     c                   endif
051800100211     c
051900100211     c                   eval      wrem=%rem(v3core:12)
052000100211     c                   if        wrem>0
052100100211     C                   MOVEL     MSG(4)        V1CMSG
052200100212     c                   seton                                        904228
052300100211     c                   goto      endct3
052400100211     c                   endif
052500100211     c
052600100212     c
052700100211     C     ENDCT3        ENDSR
052800010628      **********************************************************************
052900010628      * REGISTRAZIONE DATI IMMESSI
053000010628      **********************************************************************
053100010628     C     SR_REGISTRA   BEGSR
053200100211     c
053300100212     c* record nuovo
053400100211     c                   if        *in01
053500100211     c                   clear                   tntbe0fis
053600100211     c                   eval      tbecod='DCR'
053700100211     c                   eval      tbeapl='OP'
053800100211     c                   else
053900100211     c
054000100211     c* Se record esistente, chaino il fisico
054100100212     c     v2hnrr        chain     tntbe00f
054200100212     c                   endif
054300100211     c
054400100211     c                   eval      tbeke1=v1cksc
054500100211
054600100211     c                   clear                   tbeke2
054700100211     c                   select
054800100211     c                   when      v3cctr<>*blanks
054900100212     c                   eval      tbeke2=v3cctr
055000100211     c                   when      v3cpart<>*blanks
055100100211     c                   eval      tbeke2='TTR'
055200100211     c                   endsl
055300100211     c
055400100211     c                   clear                   ddcr
055500100211     c                   eval      §dcrtcr=v3ctcr
055600100211     c                   eval      §dcrore=v3core
055700100211     c                   movel(p)  ddcr          tbeuni
055800100211     c
055900100211     c                   clear                   tbeftr
056000100211     c                   eval      tbeftt='S'
056100100212     c* Annullamento
056200100224     c                   if        *inkq=*on
056300100212     c                   eval      tbeatb='A'
056400100212     c                   endif
056500100212     c* Ripristino
056600100212     c                   if        *inke=*on
056700100212     c                   eval      tbeatb=' '
056800100212     c                   endif
056900100211     c
057000100211     c                   if        *in01
057100100211     c                   write     tntbe0fis
057200100211     c                   else
057300100211     c                   update    tntbe0fis
057400100211     c                   endif
057500010628
057600040625     C                   ENDSR
057700010628      *****************************************************************
057800010628      * ROUTINE INIZIALE
057900010628      *****************************************************************
058000010628     C     *INZSR        BEGSR
058100010628      *
058200010628     C     *ENTRY        PLIST
058300010628     C                   PARM                    KPJBA
058400110802     c                   parm                    TNTA61ds
058500100210      *
058600100210      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
058700100210     c     *dtaara       define    §azute        azuteds
058800100210     c     *dtaara       define    §datiute      ddatiute
058900100210      *
059000100210     c                   clear                   AzUteDs
059100100210     c                   clear                   DDatiUte
059200100210     c                   clear                   Tibs34Ds
059300100210     c                   in(E)     *dtaara
059400100210if  1c                   if        %Error  or  RSUT = *blanks
059500100210     c                   call      'TIBS34R'
059600100210     c                   parm                    Tibs34Ds
059700100210     c                   in        *dtaara
059800100210e   1c                   endif
059900110801     c*
060000100210     c     *iso          movel     datasys       dateu             8 0
060100010628     C*
060200100210     C     Ktbe          KLIST
060300010628     C                   KFLD                    KCOD
060400010628     C                   KFLD                    KKE1
060500100210     C     Kaco          KLIST
060600100210     C                   KFLD                    codut
060700100210     C                   KFLD                    dutkci
060800100210     C                   KFLD                    kksc
060900100224     C     KTAM          KLIST
061000100224     C                   KFLD                    kksc
061100100224     C                   KFLD                    kctr
061200010628     C*
061300010628     C                   endsr
061400010628      *---------------------------------------------------------------------------------------------
061500010628** MSG  Lungh. 78                                                            *
061600100210Codice Cliente obbligatorio                                                   1
061700100211Codice Cliente errato                                                         2
061800100211Codice Cliente non trovato nel piano dei conti o bloccato                     3
061900100212Numero di ore obbligatorio: deve essere un multiplo di 12                     4
062000100211Immettere O il codice tariffa O la particolarità consegna                     5
062100100224Record già presente in tabella con le stesse caratteristiche anche annullato! 6
062200100224Già presente per il cliente un record con cod.tar o particolarità anche annull
062300100224Codice tariffa errato                                                         8
062400100224codice tariffa inesistente per il cliente                                     9
