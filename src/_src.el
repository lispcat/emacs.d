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

;; This file loads files from this directory.

;;; Code:

;; misc
(+require-or-load '+log)
(+require-or-load '+tools)

;; regular
(+require-or-load '+base)
(+require-or-load '+kbd)
(+require-or-load '+completion)
(+require-or-load '+ide)
(+require-or-load '+org)
(+require-or-load '+latex)
(+require-or-load '+workspaces)
(+require-or-load '+programs)
(+require-or-load '+ui)
(+require-or-load '+misc)
(+require-or-load '+to-sort)
(+require-or-load '+documentation)

;;; end
(provide '_src)
;;; _src.el ends here
