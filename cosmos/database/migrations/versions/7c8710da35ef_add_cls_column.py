"""Add cls column

Revision ID: 7c8710da35ef
Revises: de8dcadbd2cb
Create Date: 2020-03-17 03:07:53.246744

"""
from alembic import op
import sqlalchemy as sa
from sqlalchemy import Column, Integer, String, JSON, LargeBinary, ForeignKey, Boolean, Numeric, Text


# revision identifiers, used by Alembic.
revision = '7c8710da35ef'
down_revision = 'de8dcadbd2cb'
branch_labels = None
depends_on = None


def upgrade():
    op.add_column('object_contexts', Column('cls', String(200)))
