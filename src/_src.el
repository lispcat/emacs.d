;;; _src.el ---                                      -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lispcat

;; Author: lispcat <187922791+lispcat@users.noreply.github.com>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; misc
(+require-or-load '+log)
(+require-or-load '+tools)

;; regular
(+require-or-load 'my-base)
(+require-or-load 'my-kbd)
(+require-or-load 'my-completion)
(+require-or-load 'my-ide)
(+require-or-load 'my-org)
(+require-or-load 'my-latex)
(+require-or-load 'my-workspaces)
(+require-or-load 'my-programs)
(+require-or-load 'my-ui)
(+require-or-load 'my-misc)
(+require-or-load 'my-to-sort)
(+require-or-load 'my-documentation)

;;; end
(provide '_src)
;;; _src.el ends here
