000100160405     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400090122     Ftivgdtmp  uF   E             DISK
000500090323     FTNVASXLS  uF A E             DISK    commit
000600110328     FTITAA30C  IF   E           K DISK
000700991027
000800090122     D tnvasds       e ds                  extname(tnvas00t)
000900160414     D DTA4A         E DS
001000160414     D DTA4M         E DS
001100991027     D prmppt          s             50
001200991027     D prmesito        s              1
001300991027     D wrkesito        s                   like(prmesito)
001400060720
001500060720     D wDlmTxt         s              5    varying inz('')
001600160414
001700160414     D*--------------------
001800160414     D* DEFINIZIONI A PROCEDURE ESTERNE
001900160414     D*--------------------
002000160414     D/COPY GAITRASRC/SRCPROTOPI,UBTA400R
002100160414
002200060720
002300991027
002400030908     C                   EXSR      chkRECFILMBR
002500991027     C                   EXSR      traduci
002600160419     C*
002700160419     C* Finita l'elaborazione chiamo driver reperimento TITA4 in chiusura
002800160419     C                   CALL      'UBTA400R'
002900160419     C                   PARM                    UBTA4IOPZ
003000160419     C                   PARM      'C'           UBTA4ITLA
003100160419     C                   PARM                    UBTA4IAAS
003200160419     C                   PARM                    UBTA4ILNP
003300160419     C                   PARM                    UBTA4INRS
003400160419     C                   PARM                    UBTA4INSP
003500160419     C                   PARM                    UBTA4ITRC
003600160419     C                   PARM                    UBTA4OERR
003700160419     C                   PARM                    UBTA4ODS
003800160419     C                   PARM                    UBTA4OLEN
003900160419     C                   PARM                    UBTA4ODATI
004000160419     C*
004100921023     C                   SETON                                        LR
004200030908
004300030908     C     chkRECFILMBR  BEGSR
004400030908     C*
004500030908     C                   READ      TNVASXLS                               55
004600030908     C*
004700030908     C                   ENDSR
004800991027
004900991027     C     traduci       BEGSR
005000040206     C*
005100040206     C* Verifico se richiesto un particolare carattere d separatore campi
005200040206     C                   IF        %subst(prmppt:3:1) <> *blanks
005300040206     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
005400040206     C                   ELSE
005500160405     C                   MOVEL     ';'           wSepCampi         1
005600040206     C                   ENDIF
005700060720     C*
005800060720     C* Verifico se richiesto nessun delimitatore testo
005900060720     C*   => *blanks = standard = "
006000060720     C*   => N       = nessun delimitatore testo
006100060720     C                   EVAL      wDlmTxt = '"'
006200060720     C                   IF        %subst(prmppt:2:1) = 'N'
006300060720     C                   EVAL      wDlmTxt = *blanks
006400060720     C                   EVAL      wDlmTxt = %trim(wDlmTxt)
006500060720     C                   ENDIF
006600030325     C*
006700030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
006800030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
006900060720     C                   EVAL      VASXDTA =
007000060720     C                                wDlmTxt+'VASLNP'+wDlmTxt+wSepCampi+
007100060720     C                                wDlmTxt+'VASLNA'+wDlmTxt+wSepCampi+
007200060720     C                                wDlmTxt+'VASAAS'+wDlmTxt+wSepCampi+
007300060720     C                                wDlmTxt+'VASNRS'+wDlmTxt+wSepCampi+
007400060720     C                                wDlmTxt+'VASNSP'+wDlmTxt+wSepCampi+
007500060720     C                                wDlmTxt+'VASRMN'+wDlmTxt+wSepCampi+
007600060720     C                                wDlmTxt+'VASCAS'+wDlmTxt+wSepCampi+
007700060720     C                                wDlmTxt+'VASVCA'+wDlmTxt+wSepCampi+
007800060720     C                                wDlmTxt+'VASNRA'+wDlmTxt+wSepCampi+
007900060720     C                                wDlmTxt+'VASDTE'+wDlmTxt+wSepCampi+
008000060720     C                                wDlmTxt+'VASTPI'+wDlmTxt+wSepCampi+
008100060720     C                                wDlmTxt+'VASABI'+wDlmTxt+wSepCampi+
008200060720     C                                wDlmTxt+'VASCAI'+wDlmTxt+wSepCampi+
008300060720     C                                wDlmTxt+'VASNDP'+wDlmTxt+wSepCampi+
008400060720     C                                wDlmTxt+'VASDDP'+wDlmTxt+wSepCampi+
008500060720     C                                wDlmTxt+'VASCCB'+wDlmTxt+wSepCampi+
008600060720     C                                wDlmTxt+'VASABP'+wDlmTxt+wSepCampi+
008700060720     C                                wDlmTxt+'VASCAP'+wDlmTxt+wSepCampi+
008800060720     C                                wDlmTxt+'VASFPC'+wDlmTxt+wSepCampi+
008900060720     C                                wDlmTxt+'VASDVA'+wDlmTxt+wSepCampi+
009000060720     C                                wDlmTxt+'VASKSC'+wDlmTxt+wSepCampi+
009100110328     C                                wDlmTxt+'VASRMA'+wDlmTxt+wSepCampi+
009200160405     C                                wDlmTxt+'VASRMO'+wDlmTxt+wSepCampi+
009300160405     C                                wDlmTxt+'VASNAS'+wDlmTxt
009400030325     C*
009500030325     C                   WRITE     TNVASX00
009600030325     C                   ENDIF
009700991027     C*
009800090122     C                   READ      tivgdtmp
009900090122     C                   DOW       not %eof(tivgdtmp)
010000090122     C                   movel     vgddta        tnvasds
010100110328     C*
010200110328     C* X reperire i dati del mittente originale aggancio sempre il TITAA con tipo record = 'O':
010300110328     C                   MOVEL     *blanks       wRMO             35
010400110328     C                   EVAL      taaTRC = 'O'
010500110328     C     KEYtaa30      CHAIN     titaa30c
010600110328     C                   IF        %found(titaa30c)
010700110328     C                   EVAL      wRMO = TAARSC
010800110328     C                   ENDIF
010900160405     C*
011000160414     C* X reperire i dati della natura merce lancio apposito driver
011100160414     C                   EXSR      RTVTA4
011200160414     C                   MOVEL     §TA4ANAS      wNAS             15
011300030325     C*
011400060720     C                   EVAL      VASXDTA=
011500060720     C                                   %trim(%editc(VASLNP:'Q'))+wSepCampi+
011600060720     C                                   %trim(%editc(VASLNA:'Q'))+wSepCampi+
011700060720     C                                   %trim(%editc(VASAAS:'Q'))+wSepCampi+
011800060720     C                                   %trim(%editc(VASNRS:'Q'))+wSepCampi+
011900060720     C                                   %trim(%editc(VASNSP:'Q'))+wSepCampi+
012000060720     C                                   %trim(%editc(VASRMN:'Q'))+wSepCampi+
012100060720     C                                   %trim(%editc(VASCAS:'Q'))+wSepCampi+
012200060720     C                                  wDlmTxt+%trim(VASVCA)+wDlmTxt+wSepCampi+
012300060720     C                                  wDlmTxt+%trim(VASNRA)+wDlmTxt+wSepCampi+
012400060720     C                                   %trim(%editc(VASDTE:'Q'))+wSepCampi+
012500060720     C                                  wDlmTxt+%trim(VASTPI)+wDlmTxt+wSepCampi+
012600060720     C                                   %trim(%editc(VASABI:'Q'))+wSepCampi+
012700060720     C                                   %trim(%editc(VASCAI:'Q'))+wSepCampi+
012800060720     C                                   %trim(%editc(VASNDP:'Q'))+wSepCampi+
012900060720     C                                   %trim(%editc(VASDDP:'Q'))+wSepCampi+
013000060720     C                                  wDlmTxt+%trim(VASCCB)+wDlmTxt+wSepCampi+
013100060720     C                                   %trim(%editc(VASABP:'Q'))+wSepCampi+
013200060720     C                                   %trim(%editc(VASCAP:'Q'))+wSepCampi+
013300060720     C                                  wDlmTxt+%trim(VASFPC)+wDlmTxt+wSepCampi+
013400060720     C                                   %trim(%editc(VASDVA:'Q'))+wSepCampi+
013500060720     C                                   %trim(%editc(VASKSC:'Q'))+wSepCampi+
013600110328     C                                  wDlmTxt+%trim(VASRMA)+wDlmTxt+wSepCampi+
013700160405     C                                  wDlmTxt+%trim(wRMO)+wDlmTxt+wSepCampi+
013800160405     C                                  wDlmTxt+%trim(wNAS)+wDlmTxt
013900930409     C*
014000030325     C                   WRITE     TNVASX00
014100090122     C                   DELETE    tivgd000
014200991027     C*
014300090122     C                   READ      tivgdtmp
014400030325     C                   ENDDO
014500991027     C*
014600030325     C                   EVAL      wrkesito = '0'
014700991027     C*
014800910830     C                   ENDSR
014900160414
015000160414
015100160414
015200160414     C     RTVTA4        BEGSR
015300160414     C*
015400160414     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
015500160414     C                   CLEAR                   DTA4A
015600160414     C                   CLEAR                   DTA4M
015700160414     C*
015800160414     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
015900160414     C                   CALL      'UBTA400R'
016000160414     C                   PARM      *blanks       UBTA4IOPZ
016100160414     C                   PARM      *blanks       UBTA4ITLA
016200160414     C                   PARM      vasAAS        UBTA4IAAS
016300160414     C                   PARM      vasLNP        UBTA4ILNP
016400160414     C                   PARM      vasNRS        UBTA4INRS
016500160414     C                   PARM      vasNSP        UBTA4INSP
016600160419     C                   PARM      'A'           UBTA4ITRC
016700160414     C                   PARM                    UBTA4OERR
016800160414     C                   PARM                    UBTA4ODS
016900160414     C                   PARM                    UBTA4OLEN
017000160414     C                   PARM                    UBTA4ODATI
017100160414     C*
017200160414     C                   IF        UBTA4OERR = *zeros
017300160414     C                   SELECT
017400160414     C* Gestione output tipo record 'A'
017500160414     C                   WHEN      UBTA4ODS = 'DTA4A'
017600160414     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
017700160414     C* Gestione output tipo record 'M'
017800160414     C                   WHEN      UBTA4ODS = 'DTA4M'
017900160414     C                   EVAL      DTA4M = %subst(UBTA4ODATI:1:UBTA4OLEN)
018000160414     C                   ENDSL
018100160414     C*
018200160414     C                   ENDIF
018300160414     C*
018400160414     C                   ENDSR
018500160414
018600991027
018700991027      /TITLE Operazioni iniziali.
018800991027     C     *inzsr        BEGSR
018900991027     C*
019000991027     C     *ENTRY        PLIST
019100991027     C                   parm                    prmppt
019200991027     C     wrkesito      parm      wrkesito      prmesito
019300110328     C*
019400110328     C* Definizione chiavi di lettura
019500110328     C*
019600110328     C* TITAA30C - Completa
019700110328     C     KEYtaa30      KLIST
019800110328     C                   KFLD                    vasAAS
019900110328     C                   KFLD                    vasLNP
020000110328     C                   KFLD                    vasNRS
020100110328     C                   KFLD                    vasNSP
020200110328     C                   KFLD                    taaTRC
020300991027     C*
020400991027     C                   ENDSR
