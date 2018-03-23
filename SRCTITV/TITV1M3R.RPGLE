000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090126     h dftactgrp(*NO) actgrp(*caller) bnddir('UBBNDDIR':'UBRTVNETA')
000300991027
000400090122     Ftivgdtmp  uF   E             DISK
000500090126     FTNVAS00T  O    E             DISK    extfile(LibFile01) usropn
000600090126
000700090126      **********************************************************************************************
000800090126      *  Definizione esterne
000900090126      **********************************************************************************************
001000090126     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
001100090126     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
001200090126     D/COPY GAITRASRC/SRCPROTOPR,UBUTISQL
001300090126     D/COPY GAITRASRC/SRCPROTOPI,UBUTISQL
001400090126
001500991027
001600090126     D LibFile01       S             21A   INZ
001700090126     D Libreria01      S             10A   INZ
001800090122     D tnvasds       e ds                  extname(tnvas00t)
001900991027     D prmppt          s             50
002000991027     D prmesito        s              1
002100090123     D wrkesito        s                   LIKE(prmesito)
002200090126     D parlib          s             10    INZ(*blanks)
002300090123     D parccm          s              8    INZ(*blanks)
002400090123     D parmbr          s             10    INZ(*blanks)
002500090123     D paropz          s              1    INZ(*blanks)
002600090123     D chkcall         s              1    INZ(*blanks)
002700991027
002800090126
002900090126
003000090126      /FREE
003100090126
003200090126         // Verifico il sistema AS/400 corrente
003300090126         if UBRTVNETA_Rtv(PiOutSysNeta) = 0;
003400090126            if %subst(PiOutSysNeta:1:6) = 'SETRAS';
003500090126               LibFile01  = 'GAITRA201/TNVAS00T';
003600090126               Libreria01 = 'GAITRA201';
003700090126            else;
003800090126               LibFile01  = 'GAITRAPRV/TNVAS00T';
003900090126               Libreria01 = 'GAITRAPRV';
004000090126            endif;
004100090126         endif;
004200090126
004300090126      /END-FREE
004400090126
004500090123
004600090123     C                   exsr      preela
004700090123     C*
004800090123     C                   if        chkcall = '0'
004900090123     C                   exsr      traduci
005000090123     C                   else
005100090123     C                   eval      wrkesito = '2'
005200090123     C                   endif
005300090123     C*
005400090123     C                   exsr      endela
005500090123     C*
005600921023     C                   SETON                                        LR
005700991027
005800090123
005900090123     C*-----------
006000090123     C     PREELA        BEGSR
006100090123     C*-----------
006200090123     C*
006300090123     C* Compongo il nome del membro da dare al file d output
006400090126     C                   eval      parlib = Libreria01
006500090123     C                   eval      parmbr = 'M'+%subst(prmppt:1:7)
006600090123     C                   eval      parccm = %subst(prmppt:1:7)
006700090123     C                   eval      paropz = '1'
006800090123     C* Effettuo la chiamata al CLLE preposto
006900090123     C                   call(e)   'TITVVS0C'
007000090123     C                   parm                    parccm
007100090126     C                   parm                    parlib
007200090123     C                   parm                    parmbr
007300090123     C                   parm                    paropz
007400090123     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
007500090123     C                   if        %error
007600090123     C                   movel     '1'           chkcall
007700090123     C                   else
007800090123     C                   movel     '0'           chkcall
007900090123     C                   endif
008000090123     C*
008100090123     C                   ENDSR
008200090123
008300090123
008400090123
008500090123     C*-----------
008600090123     C     ENDELA        BEGSR
008700090123     C*-----------
008800090123     C*
008900090123     C                   if        %open(tnvas00t)
009000090123     C                   close     tnvas00t
009100090123     C                   endif
009200090123     C*
009300090123     C* Effettuo la chiamata al CLLE preposto
009400090123     C                   call(e)   'TITVVS0C'
009500090123     C                   parm                    parccm
009600090126     C                   parm                    parlib
009700090123     C                   parm                    parmbr
009800090123     C                   parm      '2'           paropz
009900090123     C*
010000090123     C                   ENDSR
010100090123     C***
010200090123
010300090123
010400090123
010500991027     C     traduci       BEGSR
010600090123     C*
010700090123     C                   if        not %open(tnvas00t)
010800090123     C                   open      tnvas00t
010900090123     C                   endif
011000991027     C*
011100090122     C                   READ      tivgdtmp
011200991027     C                   dow       not %eof
011300090122     C                   movel     vgddta        tnvasds
011400930409     C*
011500090123     C                   WRITE     TNVAS000
011600090122     C                   delete    tivgd000
011700991027     C*
011800090122     C                   READ      tivgdtmp
011900991027     C                   ENDdo
012000991027     C*
012100991027     C                   eval      wrkesito = '0'
012200991027     C*
012300910830     C                   ENDSR
012400991027
012500090123
012600991027      /TITLE Operazioni iniziali.
012700991027     C     *inzsr        BEGSR
012800991027     C*
012900991027     C     *ENTRY        PLIST
013000991027     C                   parm                    prmppt
013100991027     C     wrkesito      parm      wrkesito      prmesito
013200991027     C*
013300991027     C                   ENDSR
