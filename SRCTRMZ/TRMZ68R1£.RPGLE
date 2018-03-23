000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200100202     H* TRMZ68R1 *---------------------------------------------------*
000300100202      *          AGGIORNA AITRA nome autista e FIAPD RAG.SOC FATTURAZIONE
000400000000     H*--------------------------------------------------------------*
000500100202     Ftrmz68d1  CF   E             WORKSTN
000600091028      *
000700100202     Faitra03L  uF   E           K DISK
000701100202     Faitra05L  uF   E           K DISK    rename(aitra000:aitra5)
000800091028      *
001000100202     Ffiapd01L  uf   E           K DISK
001100090102      *
001300090410     C*****************************************************************
001400091028     D kpjba         E DS
001401120713     d exv1ccod        s                   like(v1ccod)
001800120713     d exv1ctip        s                   like(v1ctip)
001801170511     d fnlv36ds      e ds                  inz
001802170511     d fiapdds       e ds                  extname(fiapd00f)
001803170511     d fiapda        e ds                  extname(fiapd00f)
001804170511     d                                     prefix(a)
001805170511     d fiapdb        e ds                  extname(fiapd00f)
001806170511     d                                     prefix(b)
001807170511     d fiapd4A       e ds                  extname(fiapd40f)
001808170511     d                                     prefix(a)
001809170511     d fiapd4b       e ds                  extname(fiapd40f)
001810170511     d                                     prefix(b)
001811120713     d*-----------------------------------------------------------------
001900091023     c     *entry        plist
002000091023     c                   parm                    kpjba
002100091028     C                   SETON                                        LR
002101170511     c                   clear                   fiapdb
002102170511     c                   clear                   fiapda
002200091023      *
002300091028     c     inizio        tag
002400100202     c                   clear                   v1ccod
002500091028      *
002600091028     c     video         tag
002700091028     c                   setoff                                       99
002800100202      *
002900091028     c                   exfmt     video1
003000100202      *
003100091028     c   KC              goto      fine
003200081110      *
003201120713     c                   if        v1ccod <> exv1ccod or
003202120713     c                             v1ctip <> exv1ctip or
003203120713     c                             *in99
003300120713     c                   exsr      contr1
003301120713     c                   endif
003400091028     c  nKF
003500091028     cor 99              goto      video
003600091028      *
004600091028     c                   exsr      esegui
004700091028      *
004800091028     C     FINE          TAG
004900091028     c                   return
005000091028     C*****************************************************************
005100091028      *  Controlli su nr.contratto o codice aut
005200091028     C*****************************************************************
005300091028     c     contr1        begsr
005400091028      *
005500091028     c                   setoff                                       414243
005600091028      *
005700100202      *  codice autista
005800100202     c                   if        v1ccod = 0 or v1ctip = *blank
005900091028     c                   seton                                        4199
006000091028     c                   else
006100091028      *
006101100202     c                   if        v1ctip = 'C'
006102100222     c                   eval      v2ccost = 'Nome Aut.fatt. Af/de'
006104100202     c                   move      'A'           kapdtip
006105100222     c                   move      'D'           kapdtip2
006200100202     c     kaitra        chain     aitra03l
006300100202     c                   if        not %found(aitra03l)
006400100329     c                   seton                                        42
006500100202     c                   end
006501100222     c                   z-add     trabpt        ccod2
006502100202     c                   else
006503100222     c                   eval      v2ccost = 'Nome Aut.fatt. città'
006504100202     c                   move      'D'           kapdtip
006505100222     c                   move      'A'           kapdtip2
006700100202     c     kaitra        chain     aitra05l
006800100202     c                   if        not %found(aitra05l)
006900100329     c                   seton                                        42
007000091028     c                   end
007001100222     c                   z-add     trakau        ccod2
007200091028     c                   endIf
007201100202     c                   movel     traaut        v1caut
007202100202     c     kapd          chain     fiapd01l
007203100202     c                   if        not %found(fiapd01l)
007204100202     c                   seton                                        4399
007205100202     c                   else
007206100202     c                   movel     apdrsf        v1cnof
007207100202     c                   movel     apdrsc        v1cnom
007208100202     c                   endif
007209100222     c                   clear                   v1cnof2
007210100222     c     kapd2         chain     fiapd01l
007211100222     c                   if        %found(fiapd01l)
007213100222     c                   movel     apdrsf        v1cnof2
007215100222     c                   endif
007400091028      *
007401100202     c                   endif
007402120713     c                   move      v1ccod        exv1ccod
007403120713     c                   move      v1ctip        exv1ctip
007500091028     c                   endsr
007600091028     C*****************************************************************
007700091028      *    Esegue l'aggiornamento
007800091028     C*****************************************************************
007900091028     c     esegui        begsr
008000100202      * aggiorna fiapd
008001100330     c     kapd          chain     fiapd01l
008002100330     c                   if        %found(fiapd01l)
008003170511     c                   eval      fiapdB = fiapdds
008004100202     c                   eval      apdrsf = v1cnof
008005100202     c                   update    fiapd000
008006170511     c                   eval      fiapdA = fiapdds
008008170511     c                   if        fiapdA <> fiapdB
008009170511     c                   exsr      variazione
008010170511     c                   endif
008011100330     c                   endif
012101100202      * aggiorna aitra
012102100202     c                   eval      traaut = apdrsf
012103100329     c                   if        not *in42
012104100202     c                   if        v1ctip = 'C'
012400091028     c                   update    aitra000
012401100202     c                   else
012402100202     c                   update    aitra5
012403100202     c                   endif
012404100329     c                   endif
013000091023      *
013100091028     c                   ENDsr
013101170511     C*------------------------------------------------------------------------*
013102170511     C* richiama pgm variazioni
013103170511     C*------------------------------------------------------------------------*
013104170511     C     variazione    BEGSR
013105170511      * preparazione dati richiamo PGM variazioni per ANNULLAMENTO
013106170511     c                   clear                   fnlv36ds
013109170511     c                   clear                   fiapd4b
013110170511     c                   clear                   fiapd4A
013111170511     c                   eval      ILV36PRU  = knmus
013112170511     c                   eval      ILV36NOJ  = knmeb
013113170511     c                   eval      ILV36PGM  = 'TRMZ68R1'
013114170511     c                   eval      ILV36CTA  = 'M'
013116170511      *richiama pgm registrazione variazioni
013117170511     c                   call      'FNLV36R'
013118170511     c                   parm                    fnlv36ds
013119170511     c                   parm                    fiapdb
013120170511     c                   parm                    fiapda
013121170511     c                   parm                    fiapd4b
013122170511     c                   parm                    fiapd4A
013123170511     c                   ENDsr
013200081209     C*------------------------------------------------------------------------*
013300081209     C* *INZSR - OPERAZIONI INIZIALI
013400081209     C*------------------------------------------------------------------------*
013500081209     C     *INZSR        BEGSR
013600081209      *
013700081209     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
013800081209     C                   clear                   UDATE_8           8 0
013900081209     C                   MOVEL     *DATE         UDATE_8
014000081209     C*
014001100202     c     kaitra        klist
014002100202     c                   kfld                    v1ccod
014003100202     c                   kfld                    data0
014004100202     c                   z-add     0             data0             8 0
014200091028     C*
014201100202     c     kapd          klist
014202100202     c                   kfld                    kapdtip           1
014203100202     c                   kfld                    v1ccod
014204100222     c     kapd2         klist
014205100222     c                   kfld                    kapdtip2          1
014206100222     c                   kfld                    ccod2             7 0
014300081209     c                   endSR
