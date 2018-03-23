000100100317     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200100317     H*PARMS COMMIT(*NONE) DATFMT(*ISO) RDB(*NONE) ACTGRP(QILE)
000300100317     H*PARMS DYNUSRPRF(*OWNER)
000400100317     H DECEDIT('0,') DATEDIT(*DMY)
000500061003     F*--------------------------------------------------------------*
000600061003     F**                                                            **
000700130926     F**           - Wrapper
000800061003     F**                                                            **
000900061003     F*--------------------------------------------------------------*
001000061003     D* Passaggio Parametri
001100061003     D KPJBA         E DS
001200130926
001300130926     D Proj_Ndcl45r    PR                  EXTPGM('PJCI1O3/NDCL45R')
001400130926     D  kpjba                              LIKEDS(kpjba)
001500130926
001600130926      * Reperimento nome PGM
001700061003     D STATUS         SDS           333
001800061003     D  DSPGM            *PROC
001900061003     D  PARMS            *PARMS
002000090421     D  SDSUSR               254    263
002100090421     D  JOB_NUM              264    269S 0
002200061003     C*----------------------------------------------------*
002300061003     C* Ciclo principale
002400061003     C*----------------------------------------------------*
002500130926     C     *ENTRY        PLIST
002600130926     C                   PARM                    kpjba
002700130926      /free
002800131220          if Kcdaz = 'CL90' or KcdAz = 'YL90';
002900130926             EXEC SQL
003000130926             UPDATE qtemp/NDpil00F SET pilclisoll = '1'
003100130926             where pilclisoll<>'1' ;
003101140520
003102140520             exec sql
003103140520              delete from qtemp/ndpil00f
003104140521              where
003106140521              exists (select rcoksc from anrco00f a
003107140521              where a.rcosocieta = ndpil00f.pilcsi
003108140521              and a.rcokcc = ndpil00f.pilkcc
003109140521              and a.rcoksc = ndpil00f.pilksc
003110140521              and a.rcotecnico ='2')
003111140520                   ;
003112141215
003113141215
003114141215         //  exec sql
003115141215         //   delete from qtemp/ndpil00f
003116141215         //   where
003117141215         //   exists (select aclksc from ybacl00f y
003118141215         //   where y.aclsocieta = ndpil00f.pilcsi
003119141215         //   and y.aclkcc = ndpil00f.pilkcc
003120141215         //   and y.aclksc = ndpil00f.pilksc
003121141215         //   and substr(digits(y.aclage) , 5 , 3 )='999'
003122141215         //    )
003123141215         //        ;
003124141215         // endif;
003125141215
003128140521             exec sql
003129140521              delete from qtemp/ndpil00f
003130140521              where
003131141215              exists (select clnksc from ancln00f y
003132141215              where y.clnsocieta = ndpil00f.pilcsi
003133141215              and y.clnkcc = ndpil00f.pilkcc
003134141215              and y.clnksc = ndpil00f.pilksc
003135141215              and y.clnlineav = ''
003136141215              and y.clnfiliale = 0
003137141215              and y.clnClasAbc = '9'
003138141215               );
003139141215
003140141215          endif;
003300130926
003400130926          Proj_Ndcl45r(kpjba);
003500130926          *inlr = *on;
003600130926          return;
003700130926
003800130926      /end-free
