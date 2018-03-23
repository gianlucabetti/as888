000100010628     H DECEDIT('0,') DATEDIT(*YMD.)
000200100210      ***************************************************************************
000300100322      * Gestione forzatura giri di consegna per cliente Mittente
000400100210      ***************************************************************************
000500010628      *  RIEPILOGO INDICATORI
000600010628      ****************************************************************
000700100322      * 01 - protezione campi videata singola
000800100322      * 02 - *ON VARIAZIONE - *OFF IMMISSIONE
000900100322      * 03 - ripristino
001000100322      * 11 - RECORD ANNULLATO
001100100322      * 15 - on interrogazione
001200010628      * 28 - ERRORE GENERICO DSPF
001300010628      * 31 - GENERICO
001400010628      * 90 - RIEMISSIONE VIDEATA
001500010628      ****************************************************************
001600100323     Fazfgi00F  uF a E             DISK       rename(azfgi000:azfgi0fis)
001700100324     f                                        prefix(F_)
001800100323     Fazfgi01l  iF   E           K DISK       infds(fgi01)
001900100210     Fcnaco00f  IF   E           K DISK
002000100322     Fazorg01l  IF   E           K DISK
002100100322     Ftisic0D   CF   E             WORKSTN
002200100322     F                                        SFILE(sic0s02:sflNRR)
002300010628      *------------------------------------------------------------------------*
002400010628      *   S C H I E R E
002500100324     D MSG             S             78    DIM(18) CTDATA PERRCD(1)              MSG VIDEO
002600100324     D $pog            s              3    dim(250)
002700100324     D $DIG            s              1    dim(20)
002800100324     D $ARG            s              3    dim(50)
002900100324     d
003000100210      *
003100100322     d fidg09ds      e ds
003200100322     d TIBS34ds      e ds
003300100210     d dDatiUte      e ds
003400100210     d AZUTEds       e ds                  extname(AZUTE00F)
003500100324     d dLat          e ds
003600100324     d dute01        e ds
003700100324     D TRUL31DS      E DS
003800100324     D TRUL31DS2     E DS
003900100324     d
004000100324     D fnlv55ds      E DS
004100100324     D tibs02ds      E DS
004200100323
004300100210      *
004400100323     D fgi01           DS
004500100323     D  fgiNRR               397    400B 0
004600010628     D*
004700010628     D KPJBA         E DS
004800010628     D*
004900100322     D WLBDA8          DS                  INZ
005000100322     D  G08DAT                 1      8  0
005100100322     D  G08INV                 9     16  0
005200100322     D  G08ERR                17     17
005300100322     D  G08TGI                18     22  0
005400100322     d
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
007100100322     d klnp            s                   like(fgilnp)
007200100322     d kdde            s                   like(fgidde)
007300100210     d xx              s              2  0
007400100210     d codut           s              1  0 inz(1)
007500100212     d sflnrr          s              4  0
007600100324     d wabi            s                   like(UteAut)
007700100324     d Esci            s              1
007800100324     d Abilnp          s              1
007900100324     d Abilna          s              1
008000100210     d
008100100210     d Datasys         s               d   inz(*sys) datfmt(*iso)
008200100322     d Dataiso         s               d   datfmt(*iso)
008300100322     d Dataeur         s               d   datfmt(*eur)
008400100210       // -?Costante per controllo "caratteri solo numerici"?
008500100210     d c_Digits        c                   const('0123456789')
008600010628     D*------------------------------------------------------------------------*
008700010628     C*
008800100322      * Caricamento e pulizia sFL
008900100322     c     Inizio        tag
009000100324     c                   if        not *in28
009100010628     C                   EXSR      SR_CARVID
009200100324     c                   else
009300100324     c                   eval      Esci='S'
009400100324     c                   endif
009500100210     c
009600100211     c* Emetto sfl se trovato almeno un record
009700100322     C     forsf         TAG
009800100322     c                   if        sflnrr=0
009900100322     c                   seton                                        36
010000100322     c                   endif
010100100322     c
010200100322     c                   write     sic0t01
010300100322     c                   write     sic0z02
010400100322     C                   EXFMT     sic0c02
010500100211     C*
010600100211     c                   setoff                                       9028
010700100211     C*
010800100211      * F3=Fine
010900100324     c                   if        Esci='S' or *inkc
011000100324     C                   goto      fine
011100100324     c                   endif
011200100211     c
011300100211     c* se effettuata scelta, o "F10-Nuova Immissione" -->  emetto video 3
011400100322     c                   if        not *inkj
011500100322     c                   readc     sic0s02
011600100322     c                   endif
011700100212     c
011800100322     c                   if        vscsce='1'  or *inkj=*on
011900100322     c                   exsr      Caricavid1
012000100211     c                   else
012100100322     c                   goto      forsf
012200100211     c                   endif
012300010628     C*
012400100322      * Emissione  Videata  dettaglio
012500100322     C     for01         TAG
012600100322     c                   write     sic0t01
012700100322     C                   EXFMT     sic0d01
012800010628     C*
012900100210     c                   setoff                                       9028
013000100212     c                   setoff                                       404142
013100100322     c                   setoff                                       434445
013200010628     C*
013300010628      * F3=Fine
013400100210     C   KC              goto      fine
013500010628     C*
013600010628      * F12=Ritorno
013700100224     c                   if        *inkl
013800100323     c* pulisco il campo scelta
013900100323     c                   clear                   vscsce
014000100323     c                   if        vscann<>*blanks
014100100323     c                   seton                                        11
014200100323     c                   else
014300100323     c                   setoff                                       11
014400100323     c                   endif
014500100323     c                   update    sic0s02
014600100322     C                   goto      forsf
014700100224     c                   endif
014800010628     C*
014900100323     C  n15              EXSR      SR_CONTR01
015000100322     C   90              GOTO      for01
015100100322     c
015200100323      * F7=INTERRogazione Giri di Consegna
015300100322     C                   if        *inkg
015400100322     c                   exsr      cercagiri
015500100322     c                   goto      for01
015600100322     c                   endif
015700010628     C*
015800010628      * F6=Aggiornamento o F5=Ripristino o F16=Annullamento
015900010628     C                   IF        *INKE = *ON OR *INKF = *ON OR *INKQ = *ON
016000010629     C                   EXSR      SR_REGISTRA
016100100210     C                   goto      inizio
016200010628     C                   ENDIF
016300010628     C*
016400100322     C                   GOTO      for01
016500040625     c*
016600100210     c     fine          tag
016700100324     C                   CLEAR                   fnlv55ds
016800100324     C                   MOVEL     'C'           D55TLA
016900100324     C                   CALL      'FNLV55R'
017000100324     C                   PARM                    fnlv55ds
017100100324
017200100324     C                   CLEAR                   tibs02ds
017300100324     C                   MOVEL     'C'           T02TLA
017400100324     C                   CALL      'FNLV55R'
017500100324     C                   PARM                    tibs02ds
017600100324     C*
017700100324     c* Leggo tutti i record annullati e più vecchi di 3 mesi data variaz.
017800100324     c     *start        setll     azfgi00f
017900100324     c                   read      azfgi00f
018000100324     c
018100100324     c                   dow       not %eof(azfgi00f)
018200100324     c                   if        f_fgiatb<>*blanks and f_fgidim<datadelet
018300100324     c                   delete    azfgi0fis
018400100324     c                   endif
018500100324     c
018600100324     c                   read      azfgi00f
018700100324     c                   enddo
018800100324     c
018900010628     C                   EVAL      *INLR = *ON
019000010628      **********************************************************************
019100010628      * CONTROLLI VIDEO 1
019200010628      **********************************************************************
019300010628     C     SR_CONTR01    BEGSR
019400010628
019500100210     C* Verifico se richiesta la ricerca alfabetica
019600100322     c                   if        v1dccm<>*blanks and v1cccm<=*zeros
019700100210     c                   eval      pardut=rsut
019800100210     c                   clear                   parrag
019900100322     c                   eval      parrag=v1dccm
020000100210     c                   eval      parkcc=dutkci
020100100210     c                   eval      parsta=*zeros
020200100210     c                   clear                   parflr
020300100210     c                   clear                   pardit
020400100210     c                   eval      parnum=1
020500100210     c                   clear                   parkcm
020600100210     c                   clear                   parksm
020700100210     c                   clear                   parkdm
020800100210     c                   clear                   paresci
020900100210     c                   clear                   parerr
021000100210     c                   clear                   pariva
021100100210     c                   clear                   parcdf
021200100210     c                   call      'XALFA3BR'
021300100210     C                   PARM                    PARDUT
021400100210     C                   PARM                    codut
021500100210     C                   PARM                    parrag
021600100210     C                   PARM                    parkcc
021700100210     C                   PARM                    PARSTA
021800100210     C                   PARM                    PARFLR           90
021900100210     C                   PARM                    PARDIT            3
022000100210     C                   PARM                    PARNUM
022100100210     C                   PARM                    PARKCM
022200100210     C                   PARM                    PARKSM
022300100210     C                   PARM                    PARKDM
022400100210     C                   PARM                    PAResci
022500100210     C                   PARM                    PARerr
022600100210     C                   PARM                    PARiva
022700100210     C                   PARM                    PARcdf
022800100210     c
022900100210     c                   if        parsta<>-1 and parerr=*blanks
023000100322     c                   eval      v1cccm=parksm
023100100322     c                   eval      v1dccm=parrag
023200100210     c                   endif
023300100210
023400100322     c                   seton                                        9040
023500100210     C                   GOTO      ENDSRCONTR
023600100210     c                   endif
023700100210     c
023800100210      * Controllo se campo valorizzato
023900100322     C                   IF        v1cccm<= *ZEROS
024000100322     C                   SETON                                        289040
024100010628     C                   MOVEL     MSG(1)        V1CMSG
024200010628     C                   GOTO      ENDSRCONTR
024300010628     C                   ENDIF
024400100210     c
024500100210     c* controllo ksc
024600100322     c     c_digits      check     v1cccm                                 30
024700100211     c                   if        *in30
024800100322     C                   SETON                                        289040
024900100210     C                   MOVEL     MSG(2)        V1CMSG
025000100210     C                   GOTO      ENDSRCONTR
025100100210     C                   ENDIF
025200100210     c
025300100322     c                   movel     v1cccm        kksc
025400100210     c     kaco          chain     cnaco00f
025500100210     c                   if        not %found(cnaco00f) or acoabl<>*blanks
025600100322     C                   SETON                                        289040
025700100210     C                   MOVEL     MSG(3)        V1CMSG
025800100210     C                   GOTO      ENDSRCONTR
025900100323     c                   else
026000100323     c                   movel     acorag        v1dccm
026100100210     C                   ENDIF
026200100322     c*
026300100322     C* Linea di partenza
026400100324    1c                   if        v1clnp<>999
026500100322     c     v1clnp        chain     azorg01l
026600100324    2c                   if        not %found(azorg01l) or
026700100322     c                             (orgfag<>'A' or orgfag<>'F') or
026800100322     c                              orgfva<>' '
026900100322     C                   SETON                                        289041
027000100322     C                   MOVEL     MSG(4)        V1CMSG
027100100322     C                   GOTO      ENDSRCONTR
027200100324    2C                   ENDIF
027300100324     c* Devo essere abilitato alla linea di partenza
027400100324     c                   movel     v1clnp        w003a             3
027500100324     C     w003a         LOOKUP    $pog                                   31
027600100324    2c                   if        not *in31
027700100324     C                   SETON                                        289041
027800100324     C                   MOVEL     MSG(17)       V1CMSG
027900100324     C                   GOTO      ENDSRCONTR
028000100324    2C                   ENDIF
028100100324
028200100324    1C                   ENDIF
028300100322     c
028400100323     c* Data Decorrenza
028500100322     c                   clear                   v1hdde
028600100322     c                   if        v1cdde=0
028700100322     C                   SETON                                        289042
028800100322     C                   MOVEL     MSG(5)        V1CMSG
028900100322     C                   GOTO      ENDSRCONTR
029000100322     C                   ENDIF
029100100322     c
029200100322     C                   Z-ADD     v1cdde        G08DAT
029300100322     C                   Z-ADD     *ZEROS        G08INV
029400100322     C                   MOVEL     '0'           G08ERR
029500100322     C                   CALL      'XSRDA8'
029600100322     C                   PARM                    WLBDA8
029700100322     C                   Z-ADD     G08DAT        v1cdde
029800100322     C                   Z-ADD     G08INV        v1hdde            8 0
029900100322IF  1C     G08ERR        IFEQ      '1'
030000100322     C                   SETON                                        289042
030100100322     C                   MOVEL     MSG(6)        V1CMSG
030200100322     C                   GOTO      ENDSRCONTR
030300100322     C                   ENDIF
030400100323     c
030500100323     c* Verifico se non ci sia un record uguale
030600100323     c                   if        not *in02
030700100323     c                   movel     v1cccm        kksc
030800100323     c                   movel     v1clnp        klnp
030900100323     c                   movel     v1hdde        kdde
031000100323     c
031100100323     c     kfgi          chain     azfgi01l
031200100324    2c                   if        %found(azfgi01l)
031300100323     C                   SETON                                        289042
031400100323     C                   MOVEL     MSG(13)       V1CMSG
031500100323     C                   GOTO      ENDSRCONTR
031600100323     c                   endif
031700100323     c                   endif
031800100322     c
031900100323     c* Data Scadenza
032000100322     c                   clear                   v1hdsc            8 0
032100100322     c                   if        v1cdsc=0
032200100322     C                   SETON                                        289043
032300100322     C                   MOVEL     MSG(7)        V1CMSG
032400100322     C                   GOTO      ENDSRCONTR
032500100322     C                   ENDIF
032600100322     c
032700100322     C                   Z-ADD     v1cdsc        G08DAT
032800100322     C                   Z-ADD     *ZEROS        G08INV
032900100322     C                   MOVEL     '0'           G08ERR
033000100322     C                   CALL      'XSRDA8'
033100100322     C                   PARM                    WLBDA8
033200100322     C                   Z-ADD     G08DAT        v1cdsc
033300100322     C                   Z-ADD     G08INV        v1hdsc
033400100322IF  1C     G08ERR        IFEQ      '1'
033500100322     C                   SETON                                        289043
033600100322     C                   MOVEL     MSG(8)        V1CMSG
033700100322     C                   GOTO      ENDSRCONTR
033800100322     C                   ENDIF
033900100322     c
034000100322     c                   if        v1hdsc<v1hdde
034100100322     C                   SETON                                        289043
034200100322     C                   MOVEL     MSG(9)        V1CMSG
034300100322     C                   GOTO      ENDSRCONTR
034400100322     C                   ENDIF
034500100323     c
034600100323     c* Verifico se ci sono degli accavallamenti tra decorrenze e scadenze
034700100323     c                   movel     v1cccm        kksc
034800100323     c                   movel     v1clnp        klnp
034900100323     c     kfgi2         setll     azfgi01l
035000100323     c     kfgi2         reade     azfgi01l
035100100323     c                   dow       not %eof(azfgi01l)
035200100323     c                   if        fginrr<>vshnrr  or not *in02
035300100323     c     v1hdde        IFGE      fgiDDE
035400100323     c     v1hdde        ANDLE     fgiDSC
035500100323     c     v1hdsc        ORGE      fgiDDE
035600100323     c     v1hdsc        ANDLE     fgiDSC
035700100323     c     fgiDDE        ORGE      v1hdde
035800100323     c     fgiDSC        ANDLE     v1hdsc
035900100323     C                   SETON                                        289043
036000100323     C                   MOVEL     MSG(14)       V1CMSG
036100100323     C                   GOTO      ENDSRCONTR
036200100323     C                   ENDIF
036300100323     C                   ENDIF
036400100323     c
036500100323     c     kfgi2         reade     azfgi01l
036600100323     c                   enddo
036700100322     c
036800100322     c* Linea  codice giro
036900100322     c                   if        v1cpog<=*zeros
037000100322     C                   SETON                                        289044
037100100322     C                   MOVEL     MSG(10)       V1CMSG
037200100322     C                   GOTO      ENDSRCONTR
037300100322     C                   ENDIF
037400100324     c
037500100324     c
037600100324     c                   movel     v1cpog        w003a             3
037700100324     C     w003a         LOOKUP    $pog                                   31
037800100324    2c                   if        not *in31
037900100324     C                   SETON                                        289044
038000100324     C                   MOVEL     MSG(18)       V1CMSG
038100100324     C                   GOTO      ENDSRCONTR
038200100324    2C                   ENDIF
038300100322     c
038400100324     c* Se vuoto non controllo se premuto F7 per l'interrrogazione
038500100322     c  nkg              if        V1Ccgi = *blanks
038600100322     c                   seton                                        284590
038700100322     C                   MOVEL     MSG(11)       V1CMSG
038800100322     C                   GOTO      ENDSRCONTR
038900100322e   1c                   endif
039000100322c
039100100322      * - Codice giro consegna errato
039200100322if  1c                   IF        V1Ccgi <> *blanks
039300100322      * - - Controllo
039400100322     c                   clear                   FIDG09ds
039500100322     c                   eval      D09iop0 = '001'
039600100324     c* Come filiale gestione metto la capofila
039700100324     c                   clear                   fnlv55ds
039800100324     c                   eval      d55tpt = '6'
039900100324     c                   z-add     v1cpog        d55lin
040000100324     c                   z-add     v1hdde        d55drf
040100100324     c                   call      'FNLV55R'
040200100324     c                   parm                    fnlv55ds
040300100324     c                   z-add     d55tfa        d09ifgs
040400100324
040500100322     c                   eval      D09icgi = V1Ccgi
040600100322     c                   eval      D09idat = dateu
040700100322     c                   eval      D09itug = 'C'
040800100322     c                   eval      KPJBU   = FIDG09ds
040900100322     c                   call      'FIDG09R'
041000100322     c                   parm                    KPJBA
041100100322     c                   eval      FIDG09ds= KPJBU
041200100322if  2c                   If        D09oerr = *on
041300100322     c                   seton                                        284590
041400100322     C                   MOVEL     MSG(11)       V1CMSG
041500100322     C                   GOTO      ENDSRCONTR
041600100322x   2c                   Else
041700100322     c
041800100322if  3c                   if        D09ofgs = V1Cpog
041900100322     c                   eval      V1Dcgi  = D09odes
042000100322x   3c                   else
042100100322     c                   seton                                        284590
042200100322     C                   MOVEL     MSG(11)       V1CMSG
042300100322     C                   GOTO      ENDSRCONTR
042400100322e   3c                   endif
042500100322e   2c                   EndIf
042600100322e   1c                   ENDIF
042700100322      *
042800100324     C     ENDSRCONTR    tag
042900100324     c* se campi protetti e c'e' stato erroe --> li sproteggo per permettere ripristino
043000100324     c                   if        *in90 and *in01 and *in03
043100100324     c                   setoff                                       01
043200100324     c                   endif
043300100324     c
043400100324     C                   endsr
043500010628      **********************************************************************
043600100322      * CARICAMENTO dati
043700010628      **********************************************************************
043800010628     C     SR_CARVID     BEGSR
043900100211     c
044000100322     C                   SETON                                        3536
044100100322     C                   WRITE     sic0c02
044200100322     C                   SETOFF                                       3536
044300100322     c
044400100322     c                   if        not *in15
044500100322     c                   clear                   v1dtip
044600100322     c                   endif
044700100210     c
044800100322     c* Leggo tutti i record
044900100212     c                   clear                   sflnrr
045000100210     c
045100100322     C     *loval        setll     azfgi01l
045200100323     C                   read      azfgi01l
045300100212     c
045400100322     c                   dow       not %eof(azfgi01l)
045500100323     c
045600100324     c* Carico solo i codici a cui l'utente è abilitato come lnp e lna
045700100324     c                   eval      Abilnp='S'
045800100324     c                   eval      Abilna='S'
045900100324
046000100324     c                   if        fgilnp<>999
046100100324     c                   movel     fgilnp        w003a             3
046200100324     C     w003a         LOOKUP    $pog                                   31
046300100324    2c                   if        not *in31
046400100324     c                   eval      Abilnp='N'
046500100324     c                   endif
046600100324     c                   endif
046700100324
046800100324     c                   movel     fgipog        w003a             3
046900100324     C     w003a         LOOKUP    $pog                                   31
047000100324    2c                   if        not *in31
047100100324     c                   eval      Abilna='N'
047200100324     c                   endif
047300100323     c
047400100324    1c                   if        Abilnp='S' and Abilna='S'
047500100211     c                   exsr      Caricasfl
047600100323     c                   endif
047700100210     c
047800100323     C                   read      azfgi01l
047900100210     c                   enddo
048000010628     C*
048100010628     C                   ENDSR
048200100211      **********************************************************************
048300100211      * Carica SFL
048400100211      **********************************************************************
048500100211     C     Caricasfl     BEGSR
048600100211     c
048700100322     c                   if        fgiatb<>' '
048800100324     c                   eval      vscann='Ann.'
048900100322     c                   seton                                        11
049000100322     c                   else
049100100322     c                   eval      vscann='  '
049200100322     c                   setoff                                       11
049300100322     c                   endif
049400100322     c
049500100322     c                   movel     fgiccm        vscccm
049600100322     c                   eval      kksc=fgiccm
049700100322     c     kaco          chain     cnaco00f
049800100322     c                   if        %found(cnaco00f)
049900100322     c                   movel     acorag        vsdccm
050000100322     c                   else
050100100322     c                   movel     *all'?'       vsdccm
050200100322     c                   endif
050300100322     c
050400100322     c                   movel     fgilnp        vsclnp
050500100322     c                   movel     fgipog        vscpog
050600100322     c                   movel     fgicgi        vsccgi
050700100322     c
050800100322     c                   movel     fgidde        vshdde
050900100322     c     *iso          movel     fgidde        dataiso
051000100322     c                   movel     dataiso       dataeur
051100100322     c                   movel     dataeur       vscdde
051200100322     c
051300100322     c
051400100322     c     *iso          movel     fgidsc        dataiso
051500100322     c                   movel     dataiso       dataeur
051600100322     c                   movel     dataeur       vscdsc
051700100322     c
051800100323     c                   clear                   vscsce
051900100323     c                   eval      vshnrr=fginrr
052000100211     c
052100100212     c                   eval      sflnrr=sflnrr+1
052200100322     c                   write     sic0s02
052300100211     c
052400100211     C                   ENDSR
052500100211      **********************************************************************
052600100211      * carica dati record scelto nella videata singola
052700100211      **********************************************************************
052800100322     c     Caricavid1    BEGSR
052900100211     c
053000100322     c                   clear                   v1cccm
053100100322     c                   clear                   v1dccm
053200100322     c                   clear                   v1cdde
053300100322     c                   eval      v1cdsc=31122039
053400100322     c                   eval      v1clnp=999
053500100322     c                   clear                   v1cpog
053600100322     c                   clear                   v1ccgi
053700100323     c                   clear                   v1dcgi
053800100322     c                   clear                   v1cute
053900100322     c                   clear                   v1cdim
054000100322    1c                   if        *inkj
054100100322     c                   setoff                                       010203
054200100322     C                   eval      v1dtip= '   IMMISSIONE  '
054300100322     c
054400100322   x1c                   else
054500100211     c
054600100323     c     vshnrr        chain     azfgi00f
054700100211     c
054800100323    2c                   if        %found(azfgi00f)
054900100322
055000100211     c                   seton                                        02
055100100322     c                   setoff                                       03
055200100322    3c                   if        not *in15
055300100322     c                   setoff                                       01
055400100322     C                   eval      v1dtip= '   VARIAZIONE  '
055500100322    3c                   endif
055600100324    3c                   if        f_fgiatb<>' '
055700100322     c                   seton                                        0301
055800100322    3c                   endif
055900100211     c
056000100324     c                   movel     f_fgiccm      v1cccm
056100100324     c                   movel     f_fgiccm      kksc
056200100324     c                   movel     f_fgilnp      klnp
056300100324     c                   movel     f_fgidde      kdde
056400100322     c     kaco          chain     cnaco00f
056500100322    3c                   if        %found(cnaco00f)
056600100322     c                   movel     acorag        v1dccm
056700100322     c                   else
056800100322     c                   movel     *all'?'       v1dccm
056900100322    3c                   endif
057000100324     c                   eval      v1clnp =f_fgilnp
057100100324     c                   eval      v1cpog =f_fgipog
057200100324     c                   eval      v1ccgi =f_fgicgi
057300100322     c                   clear                   FIDG09ds
057400100322     c                   eval      D09iop0 = '001'
057500100324     c* Come filiale gestione metto la capofila
057600100324     c                   clear                   fnlv55ds
057700100324     c                   eval      d55tpt = '6'
057800100324     c                   z-add     v1cpog        d55lin
057900100324     c                   z-add     f_fgidde      d55drf
058000100324     c                   call      'FNLV55R'
058100100324     c                   parm                    fnlv55ds
058200100324     c                   z-add     d55tfa        d09ifgs
058300100322     c                   eval      D09icgi = V1Ccgi
058400100322     c                   eval      D09idat = dateu
058500100322     c                   eval      D09itug = 'C'
058600100322     c                   eval      KPJBU   = FIDG09ds
058700100322     c                   call      'FIDG09R'
058800100322     c                   parm                    KPJBA
058900100322     c                   eval      FIDG09ds= KPJBU
059000100322if  3c                   If        D09oerr = *off  and
059100100322if   c                             D09ofgs = V1Cpog
059200100322     c                   eval      V1Dcgi  = D09odes
059300100322    3c                   endif
059400100322     c
059500100324     c                   eval      v1cute =f_fgiute
059600100322     c                   eval      v1cdde =vscdde
059700100322     c                   eval      v1cdsc =vscdsc
059800100324     c     *iso          movel     f_fgidim      dataiso
059900100322     c                   movel     dataiso       dataeur
060000100322     c                   movel     dataeur       v1cdim
060100100322    2c                   endif
060200100322    1c                   endif
060300100211     c
060400100211     C                   ENDSR
060500100322      *---------------------------------------------------------------*
060600100322      * Gestione tasto funzionale F07 interrogazione giri di consegna *
060700100322      *---------------------------------------------------------------*
060800100322     c     Cercagiri     BEGSR
060900100322      *
061000100322      * Ricerca Codice Giro consegna
061100100322     c                   clear                   FIDG09ds
061200100322     c                   eval      D09iop0  = 'P01'
061300100324     c* Come filiale gestione giro metto la capofila
061400100324     c                   clear                   fnlv55ds
061500100324     c                   eval      d55tpt = '6'
061600100324     c                   z-add     v1cpog        d55lin
061700100324     c                   z-add     v1hdde        d55drf
061800100324     c                   call      'FNLV55R'
061900100324     c                   parm                    fnlv55ds
062000100324     c                   z-add     d55tfa        d09ifgs
062100100322     c                   eval      D09idat  = dateu
062200100322     c                   eval      D09itug  = 'C'
062300100322      *
062400100322     c                   eval      KPJBU    = FIDG09ds
062500100322     c                   call      'FIDG09R'
062600100322     c                   parm                    KPJBA
062700100322     c                   eval      FIDG09ds = KPJBU
062800100322      *
062900100322if  1c                   if        D09oerr  = *on
063000100322     c                   seton                                        284590
063100100322     c                   eval      V1cmsg   = Msg(12)
063200100322     c                   leavesr
063300100322x   1c                   else
063400100322     c                   eval      V1Cpog   = D09ofgs
063500100322     c                   eval      V1Ccgi   = D09ocgi
063600100322     c                   eval      V1Dcgi   = D09odes
063700100322e   1c                   endif
063800100322      *
063900100322     c                   ENDSR
064000010628      **********************************************************************
064100010628      * REGISTRAZIONE DATI IMMESSI
064200010628      **********************************************************************
064300010628     C     SR_REGISTRA   BEGSR
064400100211     c
064500100322     c* Record nuovo
064600100323     c                   if        not *in02
064700100323     c                   clear                   azfgi0fis
064800100324     c                   movel     v1cccm        f_fgiccm
064900100324     c                   movel     v1clnp        f_fgilnp
065000100324     c                   movel     v1hdde        F_fgidde
065100100322     c                   endif
065200100211     c
065300100324     c                   movel     v1hdsc        f_fgidsc
065400100324     c                   movel     v1cpog        f_fgipog
065500100324     c                   movel     v1ccgi        f_fgicgi
065600100324     c                   movel     knmus         f_fgiute
065700100324     c                   movel     datasys       f_fgidim
065800100211     c
065900100212     c* Annullamento
066000100224     c                   if        *inkq=*on
066100100324     c                   eval      f_fgiatb='A'
066200100212     c                   endif
066300100212     c* Ripristino
066400100212     c                   if        *inke=*on
066500100324     c                   eval      f_fgiatb=' '
066600100212     c                   endif
066700100211     c
066800100323     c                   if        not *in02
066900100323     c                   write     azfgi0fis
067000100211     c                   else
067100100323     c                   update    azfgi0fis
067200100211     c                   endif
067300010628
067400040625     C                   ENDSR
067500010628      *****************************************************************
067600010628      * ROUTINE INIZIALE
067700010628      *****************************************************************
067800010628     C     *INZSR        BEGSR
067900010628      *
068000010628     C     *ENTRY        PLIST
068100010628     C                   PARM                    KPJBA
068200100322     c                   movel     kpjbu         flagGES           1
068300100322     c
068400100322     c                   if        flagGES='I'
068500100322     c                   seton                                        1501
068600100322     c                   eval      v1dtip='INTERROGAZIONE'
068700100322     c                   endif
068800100210      *
068900100210      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
069000100210     c     *dtaara       define    §azute        azuteds
069100100210     c     *dtaara       define    §datiute      ddatiute
069200100210      *
069300100210     c                   clear                   AzUteDs
069400100210     c                   clear                   DDatiUte
069500100210     c                   clear                   Tibs34Ds
069600100210     c                   in(E)     *dtaara
069700100210if  1c                   if        %Error  or  RSUT = *blanks
069800100210     c                   call      'TIBS34R'
069900100210     c                   parm                    Tibs34Ds
070000100210     c                   in        *dtaara
070100100210e   1c                   endif
070200100324
070300100324     c                   Clear                   wabi
070400100324     c                   Clear                   vscmsg
070500100324     c                   Clear                   dLat
070600100324     c                   Clear                   Esci
070700100324
070800100324      * Verifica errori e autorità profilo
070900100324s   1c                   Select
071000100324      * se ho errori nei dati utente esco dal pgm
071100100324w   1c                   When      DutErr = 'E'
071200100324     c                   GoTo      Fine
071300100324      * se non c'è l'abilitazione
071400100324      * --> se 1° livello, abilitazioni al terminal
071500100324      *     se 2° livello, abilitazioni al punto operativo
071600100324      *     se sede è impossibile ma se capita mando a fine il pgm
071700100324w   1c                   When      UteAut = *Blanks
071800100324if  2c                   If        DutLpo = '1'
071900100324     c                   Eval      wabi   = 'TP'
072000100324e   2c                   EndIf
072100100324if  2c                   If        DutLpo = '2'
072200100324     c                   Eval      wabi   = 'PO'
072300100324e   2c                   EndIf
072400100324if  2c                   If        DutLpo = 'S'
072500100324     c                   GoTo      Fine
072600100324e   2c                   EndIf
072700100324      * carica le abilitazioni del profilo
072800100324x   1c                   Other
072900100324     c                   Eval      wabi = UteAut
073000100324e   1c                   EndSl
073100100324
073200100324      * controllo se ok l'abilitazione dell'utente
073300100324     c                   Clear                   Tibs02ds
073400100324     c                   Eval      T02Mod = 'C'
073500100324     c                   Eval      T02Sif = knsif
073600100324     c                   Eval      T02Cod = 'LAT'
073700100324     c                   Movel(p)  wabi          T02Ke1
073800100324     c                   Call      'TIBS02R'
073900100324     c                   Parm                    kpjba
074000100324     c                   Parm                    Tibs02ds
074100100324if  1c                   If        T02Err = *Blanks
074200100324     c                   Eval      dLat = T02Uni
074300100324e   1c                   EndIf
074400100324      * errore o non abilitato
074500100324if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
074600100324     c                   Seton                                        28
074700100324     C                   movel     msg(16)       vscmsg
074800100324   x1c                   Else
074900100324      * Reperimento dei P.O. gestibili dall'utente
075000100324     c                   clear                   TRUL31DS
075100100324     c                   clear                   TRUL31DS2
075200100324     c                   eval      I31abi = wabi
075300100324     c                   eval      I31cdi = DUTdis
075400100324     c                   eval      I31car = DUTare
075500100324     c                   eval      I31cpo = DUTpou
075600100324     c                   call      'TRUL31R'
075700100324     c                   parm                    KPJBA
075800100324     c                   parm                    TRUL31DS
075900100324     c                   parm                    TRUL31DS2
076000100324if  2c                   if        O31pog > *zeros
076100100324     c                   movea     O31pog        $pog
076200100324     c                   movea     O31arg        $arg
076300100324     c                   movea     O31dig        $dig
076400100324x   2c                   else
076500100324     c                   Seton                                        28
076600100324     c                   movel     MSG(16)       vscmsg
076700100324e   2c                   endif
076800100324    1c                   endif
076900100324     c
077000100210     c
077100100210     c     *iso          movel     datasys       dateu             8 0
077200100324     c                   movel     datasys       dataiso
077300100324     c                   subdur    3:*m          dataiso
077400100324     c                   movel     dataiso       datadelet         8 0
077500010628     C*
077600100210     C     Kaco          KLIST
077700100210     C                   KFLD                    codut
077800100210     C                   KFLD                    dutkci
077900100210     C                   KFLD                    kksc
078000100322     C     Kfgi          KLIST
078100100322     C                   KFLD                    kksc
078200100322     C                   KFLD                    klnp
078300100322     C                   KFLD                    kdde
078400100323     C     Kfgi2         KLIST
078500100323     C                   KFLD                    kksc
078600100323     C                   KFLD                    klnp
078700010628     C*
078800010628     C                   endsr
078900010628      *---------------------------------------------------------------------------------------------
079000010628** MSG  Lungh. 78                                                            *
079100100210Codice Cliente obbligatorio                                                   1
079200100211Codice Cliente errato                                                         2
079300100211Codice Cliente non trovato nel piano dei conti o bloccato                     3
079400100322Linea di partenza errata  o non immessa                                       4
079500100322Data Decorrenza obbligatoria                                                  5
079600100322Data Decorrenza errata                                                        6
079700100322Data Scadenza obbligatoria                                                    7
079800100322Data Scadenza errata                                                          8
079900100322Data Scadenza deve essere maggiore della data decorrenza                      9
080000100322Filiale giro consegna obbligatoria                                            10
080100100322Giro consegna indicato non valido o inesistente                               11
080200100322Non trovati giri consegna per la linea di arrivo indicata                     12
080300100323Per il cliente esiste già una forzatura con la stessa decorrenza              13
080400100323Per il cliente la forzatura si sovrappone ad una già esistente                14
080500100323Codice Cliente mittente non in gestione all'utente                            15
080600100324Utente non abilitato alla Gestione del programmma                             16
080700100324Utente non abilitato alla linea di Partenza indicata                          17
080800100324Utente non abilitato alla linea di Arrivo indicata                            18\\
