"""Add summary and keyword columns

Revision ID: 031c2e1e6db3
Revises: 5ed46fd078a0
Create Date: 2020-05-12 14:29:48.969333

"""
from alembic import op
import sqlalchemy as sa
from sqlalchemy import Column, Integer, String, JSON, LargeBinary, ForeignKey, Boolean, Numeric, Text


# revision identifiers, used by Alembic.
revision = '031c2e1e6db3'
down_revision = '5ed46fd078a0'
branch_labels = None
depends_on = None


def upgrade():
    op.add_column('object_contexts', Column('summary', Text()))
    op.add_column('object_contexts', Column('keywords', Text()))


def downgrade():
    op.drop_column('object_contexts', 'summary')
    op.drop_column('object_contexts', 'keywords')
