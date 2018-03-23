000100970526     H*------------------------------------------------------------------------*
000200070124     H* stampa controllo giro aut.
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500070124     Ffiapd01l  IF   E           K DISK
000600070507     Ffiapd22l  IF   E           K DISK
000700070125     Ffiapd32l  IF   E           K DISK
000800070125     Ffiapd33l  IF   E           K DISK    rename(fiapd300:fiapd330)
000900070125     FFidg14p   O    E             PRINTER OFLIND(*IN80)
001000970526     D*------------
001100020916     D* DS REPERIMENTO DATI UTENTE
001200970526     D*------------
001300020916     D TIBS34DS      E DS                                                       *Profili utente
001400020916     D DDATIUTE      E DS                                                       *Dati utente
001500020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
001600970526     D*------------
001700970526     D* DS DI PROCEDURA
001800001020     D*------------
001900070124     D fidg14ds      e ds
002000070129     D fidg09ds      e ds
002100970526     D*------------
002200970526     D* ARCHITETTURA
002300970526     D*------------
002400970526     D KPJBA         E DS
002500070126     dsavpdr           s                   like(apdpdr)
002600070126     dsavcgi           s                   like(apd3cgi)
002700070126     dsavprgg          s                   like(apd3prgg)
002800070129     dsavfgs           s                   like(apd3fgsg)
002900070126     d*avsql           s                   like(apd3sql)
003000070129     ddtaiso           s               d
003100070129     ddtaeur           s               d   datfmt(*Eur)
003200970526     C*------------------------------------------------------------------------*
003300970526     C* MAIN LINES
003400970526     C*------------------------------------------------------------------------*
003500070124     c                   if        d14ordine = 'G'
003600070124     c                   exsr      srcgi
003700070124     c                   else
003800070124     c                   exsr      srpdr
003900070124     c                   end
004000070124     c                   seton                                        lr
004100070124     C*------------------------------------------------------------------------*
004200070129     C* x aut.
004300070124     C*------------------------------------------------------------------------*
004400070124     C     srpdr         BEGSR
004500070124     c     keypdr        setll     fiapd01l
004600070124     c                   do        *hival
004700070124     c                   if        d14pdr <> 0
004800070124     c     keypdr        reade     fiapd01l
004900070124     c                   else
005000070124     c     apdtip        reade     fiapd01l
005100070124     c                   end
005200070124     c                   if        %eof(fiapd01l)
005300070124     c                   leave
005400070124     c                   end
005500070124     C* FILTRO X FLAG ANNULLAMENTO
005600070124     C                   if        apdatb <> ' '
005700070124     C                   iter                                                   *RECORD NON VALIDO
005800070124     C                   endif
005900070124     C* p.o. di gestione
006000070129     c                   movel     apdpdr        com03             3 0
006100070129     c                   if        d14fgs <>0 and
006200070129     C                             com03 > d14fgs
006300070124     C                   leave                                                  *RECORD NON VALIDO
006400070124     C                   endif
006500070124     c* aggancio giri
006600070124     c     keypdr        setll     fiapd32l
006700070124     c                   do        *hival
006800070124     c     keypdr        reade     fiapd32l
006900070124     c                   if        %eof(fiapd32l)
007000070124     c                   leave
007100070124     c                   end
007200070124     C* FILTRO X FLAG ANNULLAMENTO
007300070124     C                   if        apd3atb <> ' '
007400070124     C                   iter                                                   *RECORD NON VALIDO
007500070124     C                   endif
007600070124     C* FILTRO X ALLA DATA
007700070124     c                   if        d14dat <> 0
007800070124     C                   if        d14DAT >= apd3dde AND
007900070124     C                             d14DAT <= apd3dscad
008000070124     C                   else
008100070124     C                   iter                                                   *RECORD NON VALIDO
008200070124     C                   endif
008300070124     C                   endif
008400070129     C* p.o. giro
008500070129     c                   if        d14fgsg<> 0  and
008600070129     C                             apd3fgsg<> d14fgsg
008700070129     C                   iter                                                   *RECORD NON VALIDO
008800070129     C                   endif
008900070202     C* FILTRO X giro
009000070202     C                   if        d14cgi <> ' ' and d14cgi <> apd3cgi
009100070202     C                   iter                                                   *RECORD NON VALIDO
009200070202     C                   endif
009300070126     c*
009400070126     c                   if        apdpdr <>savpdr
009500070126     c                   clear                   savprgg
009600070126     c*                  clear                   savsql
009700070126     c                   end
009800070126     c*
009900070129     c                   if        com03 <> savfgs and savfgs <> 0
010000070129     c                   seton                                        80
010100070126     c                   end
010200070129     c                   eval      savfgs = com03
010300070129     c*
010400070201     c                   if        not *in80
010500070129     c                   if        apdpdr = savpdr and savpdr <> 0
010600070129     c                   seton                                        10
010700070129     c                   end
010800070126     c*
010900070126     c                   if        apd3prgg= savprgg and savprgg<> 0
011000070126     c                   seton                                        11
011100070126     c                   end
011200070126     c*
011300070126     c*                  if        apd3sql = savsql and savsql <> 0
011400070126     c*                  seton                                        12
011500070126     c*                  end
011600070201     c                   end
011700070201     c                   eval      savpdr = apdpdr
011800070201     c                   eval      savprgg= apd3prgg
011900070201     c*                  eval      savsql = apd3sql
012000070124     c* aggancio anagrafico giro
012100070125     c                   exsr      sranacgi
012200070125     c* stampo
012300070124     c                   seton                                        01
012400070124     c                   exsr      stampa
012500070124     c                   enddo
012600070124     c                   enddo
012700070124     c*
012800070124     c                   endsr
012900070124     C*------------------------------------------------------------------------*
013000070124     C* x giro
013100070124     C*------------------------------------------------------------------------*
013200070124     C     srcgi         BEGSR
013300070124     c                   if        d14cgi <> ' '
013400070129     c     keycgi        setll     fiapd33l
013500070124     c                   else
013600070129     c                   if        d14fgsg<> 0
013700070129     c     d14fgsg       setll     fiapd33l
013800070129     c                   else
013900070124     c     *loval        setll     fiapd33l
014000070124     c                   end
014100070129     c                   end
014200070124     c                   do        *hival
014300070124     c                   if        d14cgi <> ' '
014400070129     c     keycgi        reade     fiapd33l
014500070124     c                   else
014600070129     c                   if        d14fgsg<> 0
014700070129     c     d14fgsg       reade     fiapd33l
014800070129     c                   else
014900070124     c                   read      fiapd33l
015000070124     c                   end
015100070129     c                   end
015200070124     c                   if        %eof(fiapd33l)
015300070124     c                   leave
015400070126     c                   end
015500070124     C* FILTRO X FLAG ANNULLAMENTO
015600070124     C                   if        apd3atb <> ' '
015700070124     C                   iter                                                   *RECORD NON VALIDO
015800070124     C                   endif
015900070202     C* FILTRO X aut
016000070202     C                   if        d14pdr <> 0 and d14pdr <> apd3pdr
016100070202     C                   iter                                                   *RECORD NON VALIDO
016200070202     C                   endif
016300070124     C* FILTRO X ALLA DATA
016400070124     c                   if        d14dat <> 0
016500070124     C                   if        d14DAT >= apd3dde AND
016600070124     C                             d14DAT <= apd3dscad
016700070124     C                   else
016800070124     C                   iter                                                   *RECORD NON VALIDO
016900070124     C                   endif
017000070124     C                   endif
017100070124     c* aggancio anagrafico aut
017200070124     c                   eval      apdpdr = apd3pdr
017300070124     c     keypdr        chain     fiapd01l
017400070124     c                   if        not %found(fiapd01l)
017500070124     c                             or (%found(fiapd01l) and apdatb<> ' ')
017600070124     c                   iter
017700070124     c                   end
017800070126     c*
017900070126     c                   if        apd3cgi <>savcgi
018000070126     c                   clear                   savprgg
018100070126     c*                  clear                   savsql
018200070126     c                   end
018300070126     c*
018400070129     c                   if        apd3fgsg<>savfgs and savfgs <> 0
018500070129     c                   seton                                        80
018600070126     c                   end
018700070129     c                   eval      savfgs = apd3fgsg
018800070129     c*
018900070201     c                   if        not *in80
019000070129     c                   if        apd3cgi = savcgi and savcgi <> ' '
019100070129     c                   seton                                        10
019200070129     c                   end
019300070126     c*
019400070126     c                   if        apd3prgg= savprgg and savprgg<> 0
019500070126     c                   seton                                        11
019600070126     c                   end
019700070126     c*
019800070126     c*                  if        apd3sql = savsql and savsql <> 0
019900070126     c*                  seton                                        12
020000070126     c*                  end
020100070201     c                   end
020200070201     c                   eval      savcgi = apd3cgi
020300070201     c                   eval      savprgg= apd3prgg
020400070201     c*                  eval      savsql = apd3sql
020500070125     c* aggancio anagrafico giri
020600070125     c                   exsr      sranacgi
020700070125     c* stampo
020800070124     c                   seton                                        02
020900070124     c                   exsr      stampa
021000070124     c                   enddo
021100070124     c*
021200070124     c                   endsr
021300070124     C*------------------------------------------------------------------------*
021400070124     C* decodifico il giro
021500070124     C*------------------------------------------------------------------------*
021600070125     C     sranacgi      BEGSR
021700070124     c*
021800070129     c                   clear                   fidg09ds
021900070129     c                   eval      D09IOP0 = '001'
022000070413     c                   eval      d09idat = apd3dde
022100070129     c                   eval      d09ifgs = apd3fgsg
022200070129     c                   eval      d09icgi = apd3cgi
022300070129     c                   eval      d09iain = d14ain
022400070705     c                   eval      d09itug = 'C'
022500070129     c                   eval      kpjbu = fidg09ds
022600070129     c                   call      'FIDG09R'
022700070129     c                   parm                    kpjba
022800070129     c                   eval      fidg09ds = kpjbu
022900070129     c                   if        d09oerr = '1'
023000070129     c                   eval      prtdcgi = *all'?'
023100070129     c                   eval      prtain = *all'?'
023200070129     c                   else
023300070129     c                   eval      prtdcgi = d09odes
023400070129     c                   eval      prtain = d09oain
023500070129     c                   end
023600070124     c*
023700070124     c                   endsr
023800070124     C*------------------------------------------------------------------------*
023900070124     C* stampa
024000070124     C*------------------------------------------------------------------------*
024100070124     C     stampa        BEGSR
024200070507      *
024300070507     c* Aggancia estensione anagrafica x MAX.pesi volumi fermate
024400070507     c                   exsr      srapd22
024500070125     c* campi in stampa
024600070125     c                   eval      prtpdr = apdpdr
024700070125     c                   eval      prtdsf = apdrsc
024800070129     c                   eval      prtfgsg= apd3fgsg
024900070129     c                   eval      prtcgi = apd3cgi
025000070126     c                   eval      prtfpp = apd3fpp
025100070125     c                   eval      prtprgg= apd3prgg
025200070125     c                   eval      prtsql = apd3sql
025300070507     c                   if        ok_APD22 = 'S'
025400070507     c                   eval      prtpkl = apd2pkl
025500070507     c                   eval      prtvlu = apd2vlu
025600070507     c                   eval      prtnftl= apd2nftl
025700070507     c                   else
025800070507     c                   clear                   prtpkl
025900070507     c                   clear                   prtvlu
026000070507     c                   clear                   prtnftl
026100070507     c                   end
026200070124     c*
026300070124     c                   if        d14ordine = 'G'
026400070126     c   80              write     testag
026500070124     c                   else
026600070126     c   80              write     testaa
026700070124     c                   end
026800070125     c                   setoff                                       80
026900070126     c  n10              write     riga
027000070126     c   01              write     rigaa
027100070126     c   02              write     rigag
027200070124     c                   setoff                                       0102
027300070202     c                   setoff                                       101112
027400070124     c*
027500070124     c                   endsr
027600070124     C*------------------------------------------------------------------------*
027700070507     C* *INZSR - OPERAZIONI INIZIALI
027800070507     C*------------------------------------------------------------------------*
027900070507     C     srapd22       BEGSR
028000070507     c*
028100070507     c                   clear                   ok_APD22          1
028200070507      *
028300070507     C     KEYpdr2       setll     fiapd22l
028400070507     c                   do        *hival
028500070507     C     KEYpdr2       reade     fiapd22l
028600070507     c                   if        %Eof(fiapd22l)
028700070507     c                   Leave
028800070507     c                   end
028900070507      * se il record è interno ai parametri quindi OK
029000070507     C                   if        d14DAT >= apd2dde AND
029100070507     C                             d14DAT <= apd2dscad
029200070507     c                   eval      ok_APD22 = 'S'
029300070507     c                   Leave
029400070507     c                   end
029500070507      *
029600070507     c                   endDo
029700070507     c*
029800070507     c                   endsr
029900070507     C*------------------------------------------------------------------------*
030000070124     C* *INZSR - OPERAZIONI INIZIALI
030100070124     C*------------------------------------------------------------------------*
030200070124     C     *INZSR        BEGSR
030300970526     C*---
030400970526     C* RICEVIMENTO PARAMETRI
030500970526     C*---
030600970526     C     *ENTRY        PLIST
030700970612     C                   PARM                    KPJBA
030800070122     C*
030900070122     C                   IF        KPJBU <> *blanks
031000070124     C                   MOVEL     KPJBU         fidg14ds
031100070124     c                   else
031200070124     c                   clear                   fidg14ds
031300070124     c                   end
031400070126     c                   eval      nompgm = 'FIDG14R'
031500070126     C* REPERIMENTO DATI UTENTE
031600070126     C                   EXSR      REPDATIUTE
031700070124     C* controllo PARAMETRI DI INGRESSO
031800070126     c                   if        d14fgs = 0 and dutpou <> 046
031900070126     c                   eval      d14fgs = dutpou
032000070124     c                   end
032100070129     c                   if        d14fgsg= 0 and dutpou <> 046
032200070129     c                   eval      d14fgsg= dutpou
032300070129     c                   end
032400070124     c                   if        d14pdr = 0
032500070126     c                   if        dutpou <> 046
032600070126     c                   movel     dutpou        apdpdr
032700070126     c                   end
032800070124     c                   else
032900070124     c                   move      d14pdr        apdpdr
033000070124     c                   end
033100070129     c                   move      d14dat        dtaiso
033200070129     c                   move      dtaiso        dtaeur
033300070129     c                   move      dtaeur        prtdat
033400070125     C* fiapd01l
033500070129     C     KEYcgi        KLIST
033600070129     C                   KFLD                    d14fgsg
033700070129     C                   KFLD                    d14cgi
033800070129     C     KEYpdr        KLIST
033900070129     C                   KFLD                    apdtip
034000070129     C                   KFLD                    apdpdr
034100070124     c                   eval      apdtip = 'A'
034200070507      *
034300070507     C     KEYpdr2       KLIST
034400070507     C                   KFLD                    apd3TIPA
034500070507     C                   KFLD                    apd3PDR
034600070507     c                   KFLD                    apd3FPP
034700070507     c*
034800070125     c                   seton                                        80
034900070126     C                   MOVEL     *ALL'-'       CMP132
035000070125     C                   TIME                    TIMES             6 0
035100070125     C                   TIME                    WHDAT            12 0
035200070125     C                   MOVE      WHDAT         WOGGI             6 0
035300070124     c*
035400070119     C                   ENDSR
035500070124     C*--------------------------------------------------------------------------------------------*
035600070124     C* REPERISCE I DATI UTENTE
035700070124     C*--------------------------------------------------------------------------------------------*
035800070124     C     REPDATIUTE    BEGSR
035900070124     C*
036000070124     C* INIZIALIZZA VARIABILI DI WRK
036100070124     C                   CLEAR                   TIBS34DS
036200070124     C                   CLEAR                   AZUTEDS
036300070124     C                   CLEAR                   DDATIUTE
036400070124     C*
036500070124     C     *DTAARA       DEFINE    §azute        azuteds
036600070124     C     *DTAARA       DEFINE    §datiute      ddatiute
036700070124     C                   IN(E)     *DTAARA
036800070124if  1C                   IF        %Error
036900070124     c                   EVAL      I34Tla = 'L'
037000070124     C                   CALL      'TIBS34R'
037100070124     C                   PARM                    Tibs34Ds
037200070124     C                   IN        *DTAARA
037300070124e   1C                   ENDIF
037400070124     C*
037500070124     C                   ENDSR
