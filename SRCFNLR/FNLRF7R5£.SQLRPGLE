000100950524     H DECEDIT('0,') DATEDIT(*DMY.)
000200030710      *--------------------------------------------------------------*
000400030910     FFNFVV01L  IF   E           K DISK
000500950524     F                                     RENAME(FNFVV000:FNFVV001)
000600110518     FFiDST01L  IF   E           K DISK
000601041013     FFIQDT01L  IF   E           K DISK
000700021204     Ffiapd01L  IF   E           K DISK
000800950524      *--------------------------------------------------------------*
000900950524      *   DS
001000950524      *--------------------------------------------------------------*
001100061003     D fnfvvds       e ds                  extname(fnfvv00f)
001101061003     D dfvvspn       e ds
001102050520     D ddatiute      e ds
001200030910     D azuteds       e ds                  extname(AZUTE00F)
001300030910     D tibs34ds      E DS                  inz
001400950524     D KPJBA         E DS
001500030910     D FNLR59        E DS                  EXTNAME(FNLR59DS)
001800030908     D savndd          S                   INZ like(d59ndd)
001900030908     D savnda          S                   INZ like(d59nda)
002000950524     C*--------------------------------------------------------------*
002100950524     C*  Ciclo principale
002200950524     C*--------------------------------------------------------------*
002300950613     C*  Imposto la chiave per il posizionamento
002400950524     C                   Z-ADD     4             KNPG
002500030722     C                   Z-ADD     D59nda        Knfv
002600030722     C                   Z-ADD     D59FGS        KFGS
002700950524      *  Se ho selezionato una sola distinta mi posiziono direttamente
002800950524      *  su quella
002900950613     C     D59NDD        IFNE      *ZEROS                                       ---- 01 ----
003000950524     C     D59NDD        ANDEQ     D59NDA
003100030910     C     KFVV1         CHAIN     FNFVV01L                           31
003200030910     C                   IF        NOT *IN31
003300030909     C                             AND FVVDFV = D59DDT  and fvvfcf <> 'S'
003400030909     c                             and fvvatb <> '*'
003500030725     C                   EXSR      GESDIS
003600030725     C                   END
003700950613     C                   ELSE                                                   ---- 01 ----
003800950524      *  Altrimenti leggo in FNFVV tutti le distinte comprese entro i
003900950524      *  limiti scelti
004000950524     C                   Z-ADD     D59DDT        KDFV
004001061003     c                   if        d59bai = 'S'
004003061003     C/EXEC SQL
004004061003     C+ DECLARE A1 CURSOR FOR
004005061003     C+ SELECT fnfvv00f.* FROM fnfvv00f left outer join fltr900f
004006061003     C+ on fvvfgs = tr9fgs and fvvnpg = tr9npg and fvvnfv = tr9nfv and
004007061003     C+ fvvdfv = tr9dfv WHERE fvvfgs =:d59fgs and fvvnpg=4 and fvvdfv =
004008061003     C+ :d59ddt  and fvvatb <>'*' and fvvnfv between :d59ndd and
004009061003     C+ :d59nda and
004010061003     C+ fvvfcf <>'S' ORDER BY FLTR900F.TR9PEP, FLTR900F.TR9BAI,
004011061003     C+ FLTR900F.TR9PDC, FLTR900F.TR9NFV, FNFVV00F.FVVNFV
004015061003     C/END-EXEC
004016061003
004017061003     C/EXEC SQL
004018061003     C+ OPEN A1
004019061003     C/END-EXEC
004020061003     c                   else
004021061003     C/EXEC SQL
004022061003     C+ DECLARE b1 CURSOR FOR SELECT fnfvv00f.* FROM fnfvv00f WHERE
004023061003     C+ fvvfgs =:d59fgs and fvvnpg=4 and fvvdfv = :d59ddt and fvvatb
004024061003     C+ <>'*' and fvvnfv between :d59ndd and :d59nda and fvvfcf <>'S'
004025061003     C+ ORDER BY FNFVV00F.FVVNFV
004026061003     C/END-EXEC
004027061003
004028061003     C/EXEC SQL
004029061003     C+ OPEN b1
004030061003     C/END-EXEC
004031061003     c                   end
004032061003
004033061003     C                   do        *hival
004034061003
004035061003     c                   if        d59bai = 'S'
004036061003     C/EXEC SQL
004037061003     C+ FETCH NEXT FROM A1 INTO :fnfvvds
004038061003     C/END-EXEC
004039061003     c                   else
004040061003     C/EXEC SQL
004041061003     C+ FETCH NEXT FROM b1 INTO :fnfvvds
004042061003     C/END-EXEC
004043061003     c                   end
004044061003
004045061003     C                   select
004046061003     C                   when      sqlcod = 100
004047061003     c                   leave
004048061003     C                   when      sqlcod = 0
004049061003      * Controllo se tipo uscita distinta rientra nei limiti indicati
004050061003     C     D59FPD        IFne      *BLANKS                                      ---- 04 ----
004051061003     C     D59FPD        andne     FVVFPP                                       Tipo prest.
004052061003     c                   iter
004053061003     c                   end
004054061003     C                   exsr      gesdis
004055061003     C                   other
004056061003     C                   exsr      sqlerror
004057061003     C                   endsl
004058061003     C*
004059061003     C                   enddo
004060061003
004061061003     c                   if        d59bai = 'S'
004062061003     C/EXEC SQL
004063061003     C+ CLOSE A1
004064061003     C/END-EXEC
004065061003     c                   else
004066061003     C/EXEC SQL
004067061003     C+ CLOSE b1
004068061003     C/END-EXEC
004069061003     C                   END                                                    ---- 01 ----
006200950524     C*
006300950613     C                   END                                                    ---- 01 ----
006400950525     C*
006500950524     C                   SETON                                        LR
006600030708     C*--------------------------------------------------------------*
006700030708     C*  Gestione singola distinta
006800030708     C*--------------------------------------------------------------*
006900030708     C     GESDIS        BEGSR
007000030721     c*
007100030718     c                   do
007200030718     C                   MOVEL     'N'           D59ANM
007300030718     C                   Z-ADD     FVVNFV        KNFV
007301041013     C                   Z-ADD     FVVNFV        KNFV1
007302041013     C                   Z-ADD     FVVDFV        KDFV
007303050520     c* controllo se parcel/messaggerie
007304050523     c                   if        d59tpm <> ' '
007305050520     c                   movel     fvvspn        dfvvspn
007306050523     c                   if        (§fvvtpm = ' ' and d59tpm <> 'P') or
007307050523     c                             (§fvvtpm = 'M' and d59tpm <> 'M')
007308050520     c                   leave
007309050520     c                   end
007310050520     c                   end
007400030708     c* aggancio la distinta
007500110518     C     KDST          CHAIN     FiDST01L
007600110518     c                   if        not %found(fidst01l)
007700030708     c                   leave
007800030805     c                   else
007900030805     c* utilizzato il campo del numero delle bolle x bancali visto che non è
008000030805     c* + utilizzato in quanto le bolle vengono sempre ricalcolate
008001041013     C     KQDT          CHAIN     FIQDT01L
008002050207     c                   if        %found(FIQDT01l) and qdtatb = ' '
008100041013     c                   z-add     QDTNBNAF      d59nbl
008101041013     C                   ELSE
008102110518     c*                  z-add     dstnbn        d59nbl
008200030708     c                   end
008201041013     c                   end
008300030708     c* aggancio padroncino
008400030708     C     kapd          CHAIN     fiapd01L
008500030708     c                   if        not %found(fiapd01l)
008600030708     c                   leave
008700030708     c                   end
009500030910     C*  Richiamo pgm x GESTIRE LA SINGOLA DISTINTA
009600030910     c                   z-add     d59ndd        savndd
009700030910     c                   z-add     d59nda        savnda
009800030910     c                   z-add     fvvnfv        d59ndd
009900030910     c                   z-add     fvvnfv        d59nda
010000030909     C                   MOVEL(p)  FNLR59        KPJBU
010100030910     C                   CALL      'FNLRF7C'
010200030909     C                   PARM                    KPJBA
010300030910     c                   z-add     savndd        d59ndd
010400030910     c                   z-add     savnda        d59nda
010500030909     C                   ENDdo                                                  Trovate spunte
010600950524     C*
010700030718     C                   ENDSR
013201061003      *-----------------------------------------------
013202061003     C     sqlerror      begsr
013203061003      *-----------------------------------------------
013204061003     C*
013205061003     C                   call      'X66CHGJOB'
013206061003     C                   seton                                        lr
013207061003     C*
013208061003     C                   endsr
013300950525     C*--------------------------------------------------------------*
013400950525     C*  Operazioni iniziali
013500950525     C*--------------------------------------------------------------*
013600950525     C     *INZSR        BEGSR
013700950525     C*
013800950525     C     *ENTRY        PLIST
013900950525     C                   PARM                    KPJBA
014000030721     C                   MOVEL(P)  KPJBU         FNLR59
014100030718     c*
014200030718     c     *dtaara       define    §azute        azuteds
014300030718     c     *dtaara       define    §datiute      ddatiute
014400030718     C                   in(E)     *dtaara
014500030718     C                   IF        %Error  or  RSUT = *blanks
014600030718     C                   call      'TIBS34R'
014700030718     C                   parm                    Tibs34Ds
014800030718     C                   in        *dtaara
014900030718     c                   ENDIF
015000950525     C*  Definisco chiavi di accesso
015100030908     C     KFVV1         KLIST
015200030908     C                   KFLD                    KNPG
015300030908     C                   KFLD                    KNFV
015400030908     C                   KFLD                    KFGS
015500950525     C     KFVV2         KLIST
015600950525     C                   KFLD                    KNPG
015700950525     C                   KFLD                    KDFV
015800950525     C                   KFLD                    KFGS
015900950525     C     KDST          KLIST
015901110518     C                   KFLD                    KNPG
016000110518     C                   KFLD                    KNFV1
016100950525     C                   KFLD                    KFGS
016101041013     C     KQDT          KLIST
016102041013     C                   KFLD                    KFGS
016103041013     C                   KFLD                    KNFV1
016104041013     C                   KFLD                    KDFV
016200021204     C     Kapd          KLIST
016300021204     C                   KFLD                    apdtip
016400021204     C                   KFLD                    dstpdr
016500021204     c                   movel     'A'           apdtip
016600950525     C*  Definisco variabili
016700030728     C     *LIKE         DEFINE    FVVNPG        KNPG
016800950525     C     *LIKE         DEFINE    FVVNFV        KNFV
016801041013     C     *LIKE         DEFINE    qdtNFV        KNFV1
016900950525     C     *LIKE         DEFINE    FVVFGS        KFGS
017000950525     C     *LIKE         DEFINE    FVVDFV        KDFV
017100950525     C                   ENDSR
